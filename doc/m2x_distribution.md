# m2x_distribution

### view
Get details of an existing distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#View-Distribution-Details](https://m2x.att.com/developer/documentation/v2/distribution#View-Distribution-Details)

```erlang
m2x_distribution:view(Client, <<"DISTRIBUTION-ID">>).
```

### update
Update an existing distribution's information.
> [https://m2x.att.com/developer/documentation/v2/distribution#Update-Distribution-Details](https://m2x.att.com/developer/documentation/v2/distribution#Update-Distribution-Details)

```erlang
m2x_distribution:update(Client, <<"DISTRIBUTION-ID">>, Params).
```

### delete
Delete an existing device distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Delete-Distribution](https://m2x.att.com/developer/documentation/v2/distribution#Delete-Distribution)

```erlang
m2x_distribution:delete(Client, <<"DISTRIBUTION-ID">>).
```

### metadata
Get the custom metadata of the specified distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Read-Distribution-Metadata](https://m2x.att.com/developer/documentation/v2/distribution#Read-Distribution-Metadata)

```erlang
m2x_distribution:metadata(Client, <<"DISTRIBUTION-ID">>).
```

### update_metadata
Update the custom metadata of the specified distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Update-Distribution-Metadata](https://m2x.att.com/developer/documentation/v2/distribution#Update-Distribution-Metadata)

```erlang
m2x_distribution:update_metadata(Client, <<"DISTRIBUTION-ID">>, Params).
```

### get_metadata_field
Get the value of a single field of the custom metadata.
> [https://m2x.att.com/developer/documentation/v2/distribution#Read-Distribution-Metadata-Field](https://m2x.att.com/developer/documentation/v2/distribution#Read-Distribution-Metadata-Field)

```erlang
m2x_distribution:get_metadata_field(Client, <<"DISTRIBUTION-ID">>, K).
```

### set_metadata_field
Update the value of a single field of the custom metadata.
> [https://m2x.att.com/developer/documentation/v2/distribution#Update-Distribution-Metadata-Field](https://m2x.att.com/developer/documentation/v2/distribution#Update-Distribution-Metadata-Field)

```erlang
m2x_distribution:set_metadata_field(Client, <<"DISTRIBUTION-ID">>, K, V).
```

### devices
Retrieve list of devices added to the specified distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#List-Devices-from-an-existing-Distribution](https://m2x.att.com/developer/documentation/v2/distribution#List-Devices-from-an-existing-Distribution)

```erlang
m2x_distribution:devices(Client, <<"DISTRIBUTION-ID">>).
```

### create_device
Add a new device to an existing distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Add-Device-to-an-existing-Distribution](https://m2x.att.com/developer/documentation/v2/distribution#Add-Device-to-an-existing-Distribution)

```erlang
m2x_distribution:create_device(Client, <<"DISTRIBUTION-ID">>, Params).
```

### streams
Retrieve list of data streams associated with the specified distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#List-Data-Streams](https://m2x.att.com/developer/documentation/v2/distribution#List-Data-Streams)

```erlang
m2x_distribution:streams(Client, <<"DISTRIBUTION-ID">>).
```
