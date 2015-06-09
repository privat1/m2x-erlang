# m2x

### devices
Retrieve the list of devices accessible by the authenticated API key that meet the search criteria.
> [https://m2x.att.com/developer/documentation/v2/device#List-Search-Devices](https://m2x.att.com/developer/documentation/v2/device#List-Search-Devices)

```erlang
m2x:devices(Client).
m2x:devices(Client, Params).
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

### keys
Retrieve list of keys associated with the specified account.
> [https://m2x.att.com/developer/documentation/v2/key#List-Search-Keys](https://m2x.att.com/developer/documentation/v2/key#List-Search-Keys)

```erlang
m2x:keys(Client).
m2x:keys(Client, Params).
```

### create_key
Create a new key associated with the specified account.
> [https://m2x.att.com/developer/documentation/v2/key#Create-Key](https://m2x.att.com/developer/documentation/v2/key#Create-Key)

```erlang
m2x:create_key(Client, Params).
```
