## Bundles
Bundles can be used to control the read/write privileges on a channel

### Write only
- `bundle+{ chan }`
- `chan` is write only

### Read only
- `bundle-{ chan }`
- `chan` is read only

### No read, no write
- `bundle0{ chan }`
- `chan` can't be read or written on
- Useful in some cases
  - EG here

### bundle: bundle read write
- `bundle{ chan }`
- `chan` can be read and written on
- This is useful for other circumstances
  - EG here
