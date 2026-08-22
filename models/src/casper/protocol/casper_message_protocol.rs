//! Packet → `CasperMessageProto` dispatch (port of `package.toCasperMessageProto`).

use crate::casper::protocol::casper_message::CasperMessageProto;
use crate::casper::protocol::packet_type_tag::{PacketParseResult, PacketTypeTag};
use crate::errors::ModelsError;
use crate::proto::casper::{
    BlockHashMessageProto, BlockMessageProto, BlockRequestProto, FinalizedFringeProto,
    FinalizedFringeRequestProto, ForkChoiceTipRequestProto, HasBlockProto, HasBlockRequestProto,
    StoreItemsMessageProto, StoreItemsMessageRequestProto,
};
use crate::proto::routing::Packet;

fn decode_proto<P: prost::Message + Default>(content: &[u8]) -> PacketParseResult<P> {
    P::decode(content).map_err(|e| ModelsError::Decode(e.to_string()))
}

/// Parse a network packet into the casper message proto sum (port of `toCasperMessageProto`).
pub fn to_casper_message_proto(packet: &Packet) -> PacketParseResult<CasperMessageProto> {
    let tag = PacketTypeTag::from_tag(&packet.type_id)
        .ok_or_else(|| ModelsError::Malformed("Unrecognized packet typeId"))?;
    match tag {
        PacketTypeTag::BlockHashMessage => Ok(CasperMessageProto::BlockHashMessage(
            decode_proto::<BlockHashMessageProto>(&packet.content)?,
        )),
        PacketTypeTag::BlockMessage => Ok(CasperMessageProto::BlockMessage(decode_proto::<
            BlockMessageProto,
        >(&packet.content)?)),
        PacketTypeTag::HasBlockRequest => Ok(CasperMessageProto::HasBlockRequest(
            decode_proto::<HasBlockRequestProto>(&packet.content)?,
        )),
        PacketTypeTag::HasBlock => Ok(CasperMessageProto::HasBlock(decode_proto::<
            HasBlockProto,
        >(&packet.content)?)),
        PacketTypeTag::BlockRequest => Ok(CasperMessageProto::BlockRequest(decode_proto::<
            BlockRequestProto,
        >(&packet.content)?)),
        PacketTypeTag::ForkChoiceTipRequest => Ok(CasperMessageProto::ForkChoiceTipRequest(
            decode_proto::<ForkChoiceTipRequestProto>(&packet.content)?,
        )),
        PacketTypeTag::FinalizedFringeRequest => Ok(CasperMessageProto::FinalizedFringeRequest(
            decode_proto::<FinalizedFringeRequestProto>(&packet.content)?,
        )),
        PacketTypeTag::FinalizedFringe => Ok(CasperMessageProto::FinalizedFringe(decode_proto::<
            FinalizedFringeProto,
        >(&packet.content)?)),
        PacketTypeTag::StoreItemsMessageRequest => Ok(CasperMessageProto::StoreItemsMessageRequest(
            decode_proto::<StoreItemsMessageRequestProto>(&packet.content)?,
        )),
        PacketTypeTag::StoreItemsMessage => Ok(CasperMessageProto::StoreItemsMessage(
            decode_proto::<StoreItemsMessageProto>(&packet.content)?,
        )),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use prost::Message as _;

    #[test]
    fn rejects_unknown_tag() {
        let packet = Packet {
            type_id: "Nope".to_string(),
            content: vec![],
        };
        assert!(to_casper_message_proto(&packet).is_err());
    }

    #[test]
    fn dispatches_block_hash_message() {
        let proto = BlockHashMessageProto {
            hash: vec![1u8; 32],
            block_creator: vec![2],
        };
        let packet = Packet {
            type_id: "BlockHashMessage".to_string(),
            content: proto.encode_to_vec(),
        };
        let parsed = to_casper_message_proto(&packet).unwrap();
        assert!(matches!(parsed, CasperMessageProto::BlockHashMessage(_)));
    }
}
