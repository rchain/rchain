//! weupnp SSDP/SOAP gateway discovery (port of the `org.bitlet.weupnp` library surface used by
//! `UPnP.scala`).
//!
//! The synchronous, raw-socket approach mirrors `who_am_i.rs`: SSDP uses a multicast `UdpSocket`,
//! device descriptions + SOAP use a minimal HTTP/1.1 client over `TcpStream`, and XML is parsed with
//! `roxmltree`.

use std::io::{Read, Write};
use std::net::{TcpStream, UdpSocket};
use std::sync::Arc;
use std::time::Duration;

use roxmltree::Document;

use super::{GatewayDevice, PortMappingEntry, UPnPDevices};

const SSDP_MULTICAST: &str = "239.255.255.250:1900";
const IGD_DEVICE_TYPE: &str = "urn:schemas-upnp-org:device:InternetGatewayDevice:1";
const WAN_IP_SERVICE: &str = "urn:schemas-upnp-org:service:WANIPConnection:1";
const WAN_PPP_SERVICE: &str = "urn:schemas-upnp-org:service:WANPPPConnection:1";

// -------------------------------------------------------------------------------------------------
// SSDP discovery
// -------------------------------------------------------------------------------------------------

/// A parsed SSDP `M-SEARCH` response.
struct SsdpResponse {
    location: String,
    search_type: String,
}

/// Broadcast an `M-SEARCH` and collect the `LOCATION`/`ST` of every response (port of the weupnp
/// `GatewayDiscover.discover` multicast phase).
fn ssdp_discover() -> Vec<SsdpResponse> {
    let socket = match UdpSocket::bind("0.0.0.0:0") {
        Ok(s) => s,
        Err(_) => return Vec::new(),
    };
    let _ = socket.set_read_timeout(Some(Duration::from_secs(3)));
    let request = "M-SEARCH * HTTP/1.1\r\n\
                   HOST: 239.255.255.250:1900\r\n\
                   MAN: \"ssdp:discover\"\r\n\
                   MX: 3\r\n\
                   ST: ssdp:all\r\n\r\n";
    let _ = socket.send_to(request.as_bytes(), SSDP_MULTICAST);

    let mut results = Vec::new();
    let mut buf = [0u8; 8192];
    loop {
        match socket.recv_from(&mut buf) {
            Ok((len, _src)) => {
                let text = String::from_utf8_lossy(&buf[..len]);
                if let Some(location) = header(&text, "LOCATION") {
                    let search_type = header(&text, "ST").unwrap_or_default();
                    results.push(SsdpResponse {
                        location,
                        search_type,
                    });
                }
            }
            Err(_) => break, // read timeout
        }
    }
    results
}

/// Extract the value of an HTTP header from a raw response (case-insensitive name).
fn header(text: &str, name: &str) -> Option<String> {
    let needle = name.to_ascii_uppercase();
    text.lines().find_map(|line| {
        let (n, v) = line.split_once(':')?;
        if n.trim().eq_ignore_ascii_case(&needle) {
            Some(v.trim().to_string())
        } else {
            None
        }
    })
}

// -------------------------------------------------------------------------------------------------
// Minimal HTTP client (mirrors `who_am_i.rs::check_from_real`)
// -------------------------------------------------------------------------------------------------

/// Split a `http://host[:port]/path` URL into `(host, port, path)`.
fn split_url(url: &str) -> Option<(String, u16, String)> {
    let rest = url.strip_prefix("http://")?;
    let (authority, path) = rest.split_once('/').unwrap_or((rest, ""));
    let (host, port) = match authority.split_once(':') {
        Some((h, p)) => (h.to_string(), p.parse::<u16>().ok()?),
        None => (authority.to_string(), 80),
    };
    Some((host, port, format!("/{path}")))
}

/// HTTP GET returning the response body.
fn http_get(url: &str) -> Option<String> {
    let (host, port, path) = split_url(url)?;
    let mut stream = TcpStream::connect((host.as_str(), port)).ok()?;
    stream
        .set_read_timeout(Some(Duration::from_secs(5)))
        .ok()?;
    let request = format!("GET {path} HTTP/1.1\r\nHost: {host}\r\nConnection: close\r\n\r\n");
    stream.write_all(request.as_bytes()).ok()?;
    let mut response = Vec::new();
    stream.read_to_end(&mut response).ok()?;
    let response = String::from_utf8_lossy(&response);
    Some(response.split("\r\n\r\n").nth(1)?.to_string())
}

/// Resolve a (possibly relative) control URL against the device `LOCATION` base URL.
fn resolve_url(location: &str, control_url: &str) -> Option<String> {
    if control_url.starts_with("http://") {
        return Some(control_url.to_string());
    }
    let rest = location.strip_prefix("http://")?;
    let authority = rest.split('/').next()?;
    // Root-relative (`/path`) is joined onto the scheme + authority; otherwise the location's
    // directory is preserved.
    let base = if control_url.starts_with('/') {
        format!("http://{authority}")
    } else {
        location.to_string()
    };
    Some(format!("{base}{control_url}"))
}

/// SOAP POST returning the response body (port of the weupnp SOAP client).
fn soap_post(control_url: &str, service_type: &str, action: &str, args: &str) -> Option<String> {
    let (host, port, path) = split_url(control_url)?;
    let body = format!(
        "<?xml version=\"1.0\"?>\r\n<s:Envelope xmlns:s=\"http://schemas.xmlsoap.org/soap/envelope/\" s:encodingStyle=\"http://schemas.xmlsoap.org/soap/encoding/\"><s:Body><u:{action} xmlns:u=\"{service_type}\">{args}</u:{action}></s:Body></s:Envelope>"
    );
    let request = format!(
        "POST {path} HTTP/1.1\r\nHost: {host}\r\nContent-Type: text/xml; charset=\"utf-8\"\r\nSOAPAction: \"{service_type}#{action}\"\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
        body.len()
    );
    let mut stream = TcpStream::connect((host.as_str(), port)).ok()?;
    stream
        .set_read_timeout(Some(Duration::from_secs(5)))
        .ok()?;
    stream.write_all(request.as_bytes()).ok()?;
    let mut response = Vec::new();
    stream.read_to_end(&mut response).ok()?;
    let response = String::from_utf8_lossy(&response);
    Some(response.split("\r\n\r\n").nth(1)?.to_string())
}

/// Extract a named element's text from a SOAP/XML body.
fn xml_field(xml: &str, name: &str) -> Option<String> {
    let doc = Document::parse(xml).ok()?;
    doc.descendants()
        .find(|n| n.has_tag_name(name))
        .and_then(|n| n.text())
        .map(str::to_string)
}

// -------------------------------------------------------------------------------------------------
// XML parsing
// -------------------------------------------------------------------------------------------------

/// The device + WAN-service information needed to build a [`GatewayDevice`].
#[derive(Clone, Debug, PartialEq, Eq)]
struct DeviceInfo {
    device_type: String,
    friendly_name: String,
    model_name: String,
    manufacturer: String,
    model_description: String,
    service_type: String,
    control_url: String,
}

fn direct_text<'a, 'input>(node: &roxmltree::Node<'a, 'input>, name: &str) -> Option<String> {
    node.children()
        .find(|n| n.has_tag_name(name))
        .and_then(|n| n.text())
        .map(str::to_string)
}

fn wan_service<'a, 'input>(device: &roxmltree::Node<'a, 'input>) -> Option<(String, String)> {
    let service_list = device
        .children()
        .find(|n| n.has_tag_name("serviceList"))?;
    for service in service_list.children().filter(|n| n.has_tag_name("service")) {
        let service_type = direct_text(&service, "serviceType")?;
        if service_type == WAN_IP_SERVICE || service_type == WAN_PPP_SERVICE {
            let control_url = direct_text(&service, "controlURL")?;
            return Some((service_type, control_url));
        }
    }
    None
}

/// Parse a UPnP device-description document, returning the gateway's fields + WAN service (port of
/// the weupnp `GatewayDevice` constructor's XML parsing).
fn parse_device(xml: &str) -> Option<DeviceInfo> {
    let doc = Document::parse(xml).ok()?;
    let device = doc
        .root_element()
        .descendants()
        .find(|n| n.has_tag_name("device"))?;
    let device_type = direct_text(&device, "deviceType")?;
    let friendly_name = direct_text(&device, "friendlyName").unwrap_or_default();
    let model_name = direct_text(&device, "modelName").unwrap_or_default();
    let manufacturer = direct_text(&device, "manufacturer").unwrap_or_default();
    let model_description = direct_text(&device, "modelDescription").unwrap_or_default();
    let (service_type, control_url) = wan_service(&device)?;
    Some(DeviceInfo {
        device_type,
        friendly_name,
        model_name,
        manufacturer,
        model_description,
        service_type,
        control_url,
    })
}

// -------------------------------------------------------------------------------------------------
// Gateway device
// -------------------------------------------------------------------------------------------------

/// A discovered UPnP gateway (port of the weupnp `GatewayDevice`).
struct WeupnpGatewayDevice {
    friendly_name: String,
    model_name: String,
    manufacturer: String,
    model_description: String,
    device_type: String,
    search_type: String,
    service_type: String,
    location: String,
    control_url: String,
}

impl WeupnpGatewayDevice {
    fn new(info: DeviceInfo, search_type: String, location: String, control_url: String) -> Self {
        WeupnpGatewayDevice {
            friendly_name: info.friendly_name,
            model_name: info.model_name,
            manufacturer: info.manufacturer,
            model_description: info.model_description,
            device_type: info.device_type,
            search_type,
            service_type: info.service_type,
            location,
            control_url,
        }
    }

    fn soap(&self, action: &str, args: &str) -> Option<String> {
        soap_post(&self.control_url, &self.service_type, action, args)
    }
}

impl GatewayDevice for WeupnpGatewayDevice {
    fn friendly_name(&self) -> String {
        self.friendly_name.clone()
    }

    fn model_name(&self) -> String {
        self.model_name.clone()
    }

    fn manufacturer(&self) -> String {
        self.manufacturer.clone()
    }

    fn model_description(&self) -> String {
        self.model_description.clone()
    }

    fn device_type(&self) -> String {
        self.device_type.clone()
    }

    fn search_type(&self) -> String {
        self.search_type.clone()
    }

    fn service_type(&self) -> String {
        self.service_type.clone()
    }

    fn location(&self) -> String {
        self.location.clone()
    }

    fn external_ip_address(&self) -> String {
        self.soap("GetExternalIPAddress", "")
            .and_then(|body| xml_field(&body, "NewExternalIPAddress"))
            .unwrap_or_default()
    }

    fn is_connected(&self) -> bool {
        self.soap("GetStatusInfo", "")
            .and_then(|body| xml_field(&body, "NewConnectionStatus"))
            .map(|s| s == "Connected")
            .unwrap_or(false)
    }

    fn local_address(&self) -> String {
        // Discover the local interface address by routing a UDP socket toward the gateway.
        let (host, port, _) = match split_url(&self.location) {
            Some(v) => v,
            None => return "127.0.0.1".to_string(),
        };
        let socket = match UdpSocket::bind("0.0.0.0:0") {
            Ok(s) => s,
            Err(_) => return "127.0.0.1".to_string(),
        };
        let _ = socket.connect((host.as_str(), port));
        socket
            .local_addr()
            .map(|a| a.ip().to_string())
            .unwrap_or_else(|_| "127.0.0.1".to_string())
    }

    fn add_port_mapping(
        &self,
        external_port: i32,
        internal_port: i32,
        internal_client: &str,
        protocol: &str,
        description: &str,
    ) -> Result<bool, String> {
        let args = format!(
            "<NewRemoteHost></NewRemoteHost><NewExternalPort>{external_port}</NewExternalPort><NewProtocol>{protocol}</NewProtocol><NewInternalPort>{internal_port}</NewInternalPort><NewInternalClient>{internal_client}</NewInternalClient><NewEnabled>1</NewEnabled><NewPortMappingDescription>{description}</NewPortMappingDescription><NewLeaseDuration>0</NewLeaseDuration>"
        );
        self.soap("AddPortMapping", &args)
            .map(|_| true)
            .ok_or_else(|| "UPnP AddPortMapping failed".to_string())
    }

    fn delete_port_mapping(&self, external_port: i32, protocol: &str) -> Result<(), String> {
        let args = format!(
            "<NewRemoteHost></NewRemoteHost><NewExternalPort>{external_port}</NewExternalPort><NewProtocol>{protocol}</NewProtocol>"
        );
        self.soap("DeletePortMapping", &args)
            .map(|_| ())
            .ok_or_else(|| "UPnP DeletePortMapping failed".to_string())
    }

    fn get_generic_port_mapping_entry(&self, index: i32, entry: &mut PortMappingEntry) -> bool {
        let args = format!("<NewPortMappingIndex>{index}</NewPortMappingIndex>");
        let Some(body) = self.soap("GetGenericPortMappingEntry", &args) else {
            return false;
        };
        let Some(protocol) = xml_field(&body, "NewProtocol") else {
            return false;
        };
        entry.protocol = protocol;
        entry.external_port = xml_field(&body, "NewExternalPort")
            .and_then(|s| s.parse().ok())
            .unwrap_or(0);
        entry.internal_port = xml_field(&body, "NewInternalPort")
            .and_then(|s| s.parse().ok())
            .unwrap_or(0);
        entry.internal_client = xml_field(&body, "NewInternalClient").unwrap_or_default();
        entry.description = xml_field(&body, "NewPortMappingDescription").unwrap_or_default();
        true
    }
}

// -------------------------------------------------------------------------------------------------
// Discovery orchestration
// -------------------------------------------------------------------------------------------------

/// Discover gateway devices (port of `UPnP.discover` / weupnp `GatewayDiscover.discover`).
pub fn discover() -> UPnPDevices {
    let mut all: Vec<(String, Arc<dyn GatewayDevice>)> = Vec::new();
    let mut gateways: Vec<Arc<dyn GatewayDevice>> = Vec::new();

    for response in ssdp_discover() {
        let Some(xml) = http_get(&response.location) else {
            continue;
        };
        let Some(info) = parse_device(&xml) else {
            continue;
        };
        if info.device_type != IGD_DEVICE_TYPE {
            continue;
        }
        let Some(control_url) = resolve_url(&response.location, &info.control_url) else {
            continue;
        };
        let device = Arc::new(WeupnpGatewayDevice::new(
            info,
            response.search_type,
            response.location.clone(),
            control_url,
        ));
        // The "interface" key is approximated by the location host (display-only).
        let key = response
            .location
            .strip_prefix("http://")
            .and_then(|r| r.split('/').next())
            .unwrap_or("0.0.0.0")
            .to_string();
        all.push((key, device.clone()));
        gateways.push(device);
    }

    let valid_gateway = gateways
        .iter()
        .find(|g| g.is_connected())
        .cloned()
        .or_else(|| gateways.first().cloned());

    UPnPDevices {
        all,
        gateways,
        valid_gateway,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const DEVICE_XML: &str = r#"<?xml version="1.0"?>
<root xmlns="urn:schemas-upnp-org:device-1-0">
  <specVersion><major>1</major><minor>0</minor></specVersion>
  <device>
    <deviceType>urn:schemas-upnp-org:device:InternetGatewayDevice:1</deviceType>
    <friendlyName>Test Router</friendlyName>
    <manufacturer>ACME</manufacturer>
    <modelName>RT-1000</modelName>
    <modelDescription>ACME Router</modelDescription>
    <serviceList>
      <service>
        <serviceType>urn:schemas-upnp-org:service:WANIPConnection:1</serviceType>
        <serviceId>urn:upnp-org:serviceId:WANIPConn1</serviceId>
        <controlURL>/upnp/control/WANIPConn1</controlURL>
        <eventSubURL>/upnp/event/WANIPConn1</eventSubURL>
        <SCPDURL>/wanipconnSCPD.xml</SCPDURL>
      </service>
    </serviceList>
  </device>
</root>"#;

    #[test]
    fn parse_device_extracts_gateway_and_wan_service() {
        let info = parse_device(DEVICE_XML).unwrap();
        assert_eq!(info.device_type, IGD_DEVICE_TYPE);
        assert_eq!(info.friendly_name, "Test Router");
        assert_eq!(info.model_name, "RT-1000");
        assert_eq!(info.service_type, WAN_IP_SERVICE);
        assert_eq!(info.control_url, "/upnp/control/WANIPConn1");
    }

    #[test]
    fn parse_device_rejects_non_gateway() {
        let xml = DEVICE_XML.replace(IGD_DEVICE_TYPE, "urn:schemas-upnp-org:device:MediaRenderer:1");
        let info = parse_device(&xml).unwrap();
        assert_ne!(info.device_type, IGD_DEVICE_TYPE);
    }

    #[test]
    fn resolve_root_relative_control_url() {
        let url = resolve_url("http://192.168.1.1:5000/rootDesc.xml", "/upnp/control/WANIPConn1")
            .unwrap();
        assert_eq!(url, "http://192.168.1.1:5000/upnp/control/WANIPConn1");
    }

    #[test]
    fn resolve_absolute_control_url_passes_through() {
        let url = resolve_url(
            "http://192.168.1.1:5000/rootDesc.xml",
            "http://192.168.1.1:5000/upnp/control/WANIPConn1",
        )
        .unwrap();
        assert_eq!(url, "http://192.168.1.1:5000/upnp/control/WANIPConn1");
    }

    #[test]
    fn xml_field_extracts_text() {
        let body = r#"<s:Envelope xmlns:s="http://schemas.xmlsoap.org/soap/envelope/"><s:Body><u:GetExternalIPAddressResponse xmlns:u="urn:schemas-upnp-org:service:WANIPConnection:1"><NewExternalIPAddress>1.2.3.4</NewExternalIPAddress></u:GetExternalIPAddressResponse></s:Body></s:Envelope>"#;
        assert_eq!(xml_field(body, "NewExternalIPAddress"), Some("1.2.3.4".to_string()));
    }

    #[test]
    fn header_parsing_is_case_insensitive() {
        let resp = "HTTP/1.1 200 OK\r\nlocation: http://1.2.3.4/desc.xml\r\nST: ssdp:all\r\n\r\n";
        assert_eq!(
            header(resp, "LOCATION"),
            Some("http://1.2.3.4/desc.xml".to_string())
        );
    }
}
