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

### update_location
Update the current location of the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Update-Device-Location](https://m2x.att.com/developer/documentation/v2/device#Update-Device-Location)

```erlang
m2x_device:update_location(Client, <<"DEVICE-ID">>, Params).
```

### streams
Retrieve list of data streams associated with the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#List-Data-Streams](https://m2x.att.com/developer/documentation/v2/device#List-Data-Streams)

```erlang
m2x_device:streams(Client, <<"DEVICE-ID">>).
```

### values
List values from all data streams associated with a specific device.
> [https://m2x.att.com/developer/documentation/v2/device#List-Values-from-all-Data-Streams-of-a-Device](https://m2x.att.com/developer/documentation/v2/device#List-Values-from-all-Data-Streams-of-a-Device)

```erlang
m2x_device:values(Client, <<"DEVICE-ID">>).
m2x_device:values(Client, <<"DEVICE-ID">>, Params).
```

### post_updates
Post values to multiple streams at once.
> [https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates-Multiple-Values-to-Multiple-Streams](https://m2x.att.com/developer/documentation/v2/device#Post-Device-Updates-Multiple-Values-to-Multiple-Streams)

```erlang
m2x_device:post_updates(Client, <<"DEVICE-ID">>, Params).
```

### triggers
Retrieve list of triggers associated with the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#List-Triggers](https://m2x.att.com/developer/documentation/v2/device#List-Triggers)

```erlang
m2x_device:triggers(Client, <<"DEVICE-ID">>).
```

### create_trigger
Create a new trigger associated with the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Create-Trigger](https://m2x.att.com/developer/documentation/v2/device#Create-Trigger)

```erlang
m2x_device:create_trigger(Client, <<"DEVICE-ID">>, Params).
```
