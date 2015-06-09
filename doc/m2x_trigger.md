# m2x_trigger

### view
Get details of a specific trigger associated with an existing device.
> [https://m2x.att.com/developer/documentation/v2/device#View-Trigger](https://m2x.att.com/developer/documentation/v2/device#View-Trigger)

```erlang
m2x_trigger:view(Client, <<"DEVICE-ID">>, <<"TRIGGER-ID">>).
```

### update
Update an existing trigger associated with the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Update-Trigger](https://m2x.att.com/developer/documentation/v2/device#Update-Trigger)

```erlang
m2x_trigger:update(Client, <<"DEVICE-ID">>, <<"TRIGGER-ID">>, Params).
```

### delete, update
Update an existing trigger associated with the specified device.
> [https://m2x.att.com/developer/documentation/v2/device#Delete-Trigger](https://m2x.att.com/developer/documentation/v2/device#Delete-Trigger)

```erlang
m2x_trigger:update(Client, <<"DEVICE-ID">>, <<"TRIGGER-ID">>, Params).
m2x_trigger:delete(Client, <<"DEVICE-ID">>, <<"TRIGGER-ID">>).
```

### test
Test the specified trigger by firing it with a fake value.
> [https://m2x.att.com/developer/documentation/v2/device#Test-Trigger](https://m2x.att.com/developer/documentation/v2/device#Test-Trigger)

```erlang
m2x_trigger:test(Client, <<"DEVICE-ID">>, <<"TRIGGER-ID">>).
```
