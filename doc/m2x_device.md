# m2x_device

### view
Get details of an existing device.
> [https://m2x.att.com/developer/documentation/v2/device#View-Device-Details](https://m2x.att.com/developer/documentation/v2/device#View-Device-Details)

```erlang
m2x_device:view(Client, <<"DEVICE-ID">>).
```

### update
Update an existing device's information.
> [https://m2x.att.com/developer/documentation/v2/device#Update-Device-Details](https://m2x.att.com/developer/documentation/v2/device#Update-Device-Details)

```erlang
m2x_device:update(Client, <<"DEVICE-ID">>, Params).
```

### delete
Delete an existing device.
> [https://m2x.att.com/developer/documentation/v2/device#Delete-Device](https://m2x.att.com/developer/documentation/v2/device#Delete-Device)

```erlang
m2x_device:delete(Client, <<"DEVICE-ID">>).
```

### log
Retrieve list of HTTP requests received lately by the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#View-Request-Log](https://m2x.att.com/developer/documentation/v2/device#View-Request-Log)

```erlang
m2x_device:log(Client, <<"DEVICE-ID">>).
```

### location
Get location details of an existing device.
> [https://m2x.att.com/developer/documentation/v2/device#Read-Device-Location](https://m2x.att.com/developer/documentation/v2/device#Read-Device-Location)

```erlang
m2x_device:location(Client, <<"DEVICE-ID">>).
```

### location_history
Get location history details of an existing device.
> [https://m2x.att.com/developer/documentation/v2/device#Read-Device-Location-History](https://m2x.att.com/developer/documentation/v2/device#Read-Device-Location-History)

```erlang
m2x_device:location_history(Client, <<"DEVICE-ID">>).
m2x_device:location_history(Client, <<"DEVICE-ID">>, Params).
```

### update_location
Update the current location of the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Update-Device-Location](https://m2x.att.com/developer/documentation/v2/device#Update-Device-Location)

```erlang
m2x_device:update_location(Client, <<"DEVICE-ID">>, Params).
```

### metadata
Get the custom metadata of the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Read-Device-Metadata](https://m2x.att.com/developer/documentation/v2/device#Read-Device-Metadata)

```erlang
m2x_device:metadata(Client, <<"DEVICE-ID">>).
```

### update_metadata
Update the custom metadata of the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Update-Device-Metadata](https://m2x.att.com/developer/documentation/v2/device#Update-Device-Metadata)

```erlang
m2x_device:update_metadata(Client, <<"DEVICE-ID">>, Params).
```

### get_metadata_field
Get the value of a single field of the custom metadata.
> [https://m2x.att.com/developer/documentation/v2/device#Read-Device-Metadata-Field](https://m2x.att.com/developer/documentation/v2/device#Read-Device-Metadata-Field)

```erlang
m2x_device:get_metadata_field(Client, <<"DEVICE-ID">>, K).
```

### set_metadata_field
Update the value of a single field of the custom metadata.
> [https://m2x.att.com/developer/documentation/v2/device#Update-Device-Metadata-Field](https://m2x.att.com/developer/documentation/v2/device#Update-Device-Metadata-Field)

```erlang
m2x_device:set_metadata_field(Client, <<"DEVICE-ID">>, K, V).
```

### streams
Retrieve list of data streams associated with the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#List-Data-Streams](https://m2x.att.com/developer/documentation/v2/device#List-Data-Streams)

```erlang
m2x_device:streams(Client, <<"DEVICE-ID">>).
```

### commands
Retrieve list of recent commands sent to the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Device-s-List-of-Received-Commands](https://m2x.att.com/developer/documentation/v2/device#Device-s-List-of-Received-Commands)

```erlang
m2x_device:commands(Client, <<"DEVICE-ID">>).
```

### values
List values from all data streams associated with a specific device.
> [https://m2x.att.com/developer/documentation/v2/device#List-Values-from-all-Data-Streams-of-a-Device](https://m2x.att.com/developer/documentation/v2/device#List-Values-from-all-Data-Streams-of-a-Device)

```erlang
m2x_device:values(Client, <<"DEVICE-ID">>).
m2x_device:values(Client, <<"DEVICE-ID">>, Params).
```

### values_search
Search values from all data streams associated with a specific device.
> [https://m2x.att.com/developer/documentation/v2/device#Search-Values-from-all-Data-Streams-of-a-Device](https://m2x.att.com/developer/documentation/v2/device#Search-Values-from-all-Data-Streams-of-a-Device)

```erlang
m2x_device:values_search(Client, <<"DEVICE-ID">>, Params).
```

### post_update
Post single values to multiple streams.
> [https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates--Single-Values-to-Multiple-Streams-](https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates--Single-Values-to-Multiple-Streams-)

```erlang
m2x_device:post_update(Client, <<"DEVICE-ID">>, Params).
```

### post_updates
Post multiple values to multiple streams.
> [https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates--Multiple-Values-to-Multiple-Streams-](https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates--Multiple-Values-to-Multiple-Streams-)

```erlang
m2x_device:post_updates(Client, <<"DEVICE-ID">>, Params).
```
