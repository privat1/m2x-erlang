# m2x

### devices
Retrieve the list of devices accessible by the authenticated API key.
> [https://m2x.att.com/developer/documentation/v2/device#List-Devices](https://m2x.att.com/developer/documentation/v2/device#List-Devices)

```erlang
m2x:devices(Client).
m2x:devices(Client, Params).
```

### search_devices
Retrieve the list of devices accessible by the authenticated API key that meet the search criteria.
> [https://m2x.att.com/developer/documentation/v2/device#Search-Devices](https://m2x.att.com/developer/documentation/v2/device#Search-Devices)

```erlang
m2x:search_devices(Client).
m2x:search_devices(Client, Params).
```

### devices_catalog
Search the catalog of public devices.
> [https://m2x.att.com/developer/documentation/v2/device#List-Search-Public-Devices-Catalog](https://m2x.att.com/developer/documentation/v2/device#List-Search-Public-Devices-Catalog)

```erlang
m2x:devices_catalog(Client).
m2x:devices_catalog(Client, Params).
```

### devices_tags
Retrieve the list of device tags for the authenticated user.
> [https://m2x.att.com/developer/documentation/v2/device#List-Device-Tags](https://m2x.att.com/developer/documentation/v2/device#List-Device-Tags)

```erlang
m2x:devices_tags(Client).
```

### create_device
Create a new device.
> [https://m2x.att.com/developer/documentation/v2/device#Create-Device](https://m2x.att.com/developer/documentation/v2/device#Create-Device)

```erlang
m2x:create_device(Client, Params).
```

### commands
Retrieve the list of recent commands sent by the current user.
> [https://m2x.att.com/developer/documentation/v2/commands#List-Sent-Commands](https://m2x.att.com/developer/documentation/v2/commands#List-Sent-Commands)

```erlang
m2x:commands(Client).
m2x:commands(Client, Params).
```

### send_command
Send a command with the given name to one or more targets.
> [https://m2x.att.com/developer/documentation/v2/commands#Send-Command](https://m2x.att.com/developer/documentation/v2/commands#Send-Command)

```erlang
m2x:send_command(Client, Params).
```

### distributions
Retrieve list of device distributions accessible by the authenticated API key.
> [https://m2x.att.com/developer/documentation/v2/distribution#List-Distributions](https://m2x.att.com/developer/documentation/v2/distribution#List-Distributions)

```erlang
m2x:distributions(Client).
```

### create_distribution
Create a new device distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Create-Distribution](https://m2x.att.com/developer/documentation/v2/distribution#Create-Distribution)

```erlang
m2x:create_distribution(Client, Params).
```

### collections
Retrieve list of device collections accessible by the authenticated API key.
> [https://m2x.att.com/developer/documentation/v2/collections#List-Collections](https://m2x.att.com/developer/documentation/v2/collections#List-Collections)

```erlang
m2x:collections(Client).
```

### create_collection
Create a new device collection.
> [https://m2x.att.com/developer/documentation/v2/collections#Create-Collection](https://m2x.att.com/developer/documentation/v2/collections#Create-Collection)

```erlang
m2x:create_collection(Client, Params).
```

### keys
Retrieve list of keys associated with the specified account.
> [https://m2x.att.com/developer/documentation/v2/keys#List-Search-Keys](https://m2x.att.com/developer/documentation/v2/keys#List-Search-Keys)

```erlang
m2x:keys(Client).
m2x:keys(Client, Params).
```

### create_key
Create a new key associated with the specified account.
> [https://m2x.att.com/developer/documentation/v2/keys#Create-Key](https://m2x.att.com/developer/documentation/v2/keys#Create-Key)

```erlang
m2x:create_key(Client, Params).
```
