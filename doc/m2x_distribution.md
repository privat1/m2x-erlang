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
