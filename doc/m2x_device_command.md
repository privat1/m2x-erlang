# m2x_device_command

### view
Get details of a command as received by this device.
> [https://m2x.att.com/developer/documentation/v2/commands#Device-s-View-of-Command-Details](https://m2x.att.com/developer/documentation/v2/commands#Device-s-View-of-Command-Details)

```erlang
m2x_device_command:view(Client, <<"DEVICE-ID">>, Command).
```

### process
Mark a command as processed by this device.
> [https://m2x.att.com/developer/documentation/v2/device#Device-Marks-a-Command-as-Processed](https://m2x.att.com/developer/documentation/v2/device#Device-Marks-a-Command-as-Processed)

```erlang
m2x_device_command:process(Client, <<"DEVICE-ID">>, Command).
m2x_device_command:process(Client, <<"DEVICE-ID">>, Command, Params).
```

### reject
Mark a command as rejected by this device.
> [https://m2x.att.com/developer/documentation/v2/device#Device-Marks-a-Command-as-Rejected](https://m2x.att.com/developer/documentation/v2/device#Device-Marks-a-Command-as-Rejected)

```erlang
m2x_device_command:reject(Client, <<"DEVICE-ID">>, Command).
m2x_device_command:reject(Client, <<"DEVICE-ID">>, Command, Params).
```
