# m2x_distribution_trigger

### view
Get details of a specific trigger associated with an existing distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#View-Trigger](https://m2x.att.com/developer/documentation/v2/distribution#View-Trigger)

```erlang
m2x_distribution_trigger:view(Client, <<"DISTRIBUTION-ID">>, <<"TRIGGER-ID">>)
```

### update
Update an existing trigger associated with the specified distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Update-Trigger](https://m2x.att.com/developer/documentation/v2/distribution#Update-Trigger)

```erlang
m2x_distribution_trigger:update(Client, <<"DISTRIBUTION-ID">>, <<"TRIGGER-ID">>, Params)
```

### delete, update
Update an existing trigger associated with the specified distribution.
> [https://m2x.att.com/developer/documentation/v2/distribution#Delete-Trigger](https://m2x.att.com/developer/documentation/v2/distribution#Delete-Trigger)

```erlang
m2x_distribution_trigger:update(Client, <<"DISTRIBUTION-ID">>, <<"TRIGGER-ID">>, Params)
m2x_distribution_trigger:delete(Client, <<"DISTRIBUTION-ID">>, <<"TRIGGER-ID">>)
```

### test
Test the specified trigger by firing it with a fake value.
> [https://m2x.att.com/developer/documentation/v2/distribution#Test-Trigger](https://m2x.att.com/developer/documentation/v2/distribution#Test-Trigger)

```erlang
m2x_distribution_trigger:test(Client, <<"DISTRIBUTION-ID">>, <<"TRIGGER-ID">>)
```
