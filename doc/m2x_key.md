# m2x_key

### view
Get details of the specified key.
> [https://m2x.att.com/developer/documentation/v2/keys#View-Key-Details](https://m2x.att.com/developer/documentation/v2/keys#View-Key-Details)

```erlang
m2x_key:view(Client, <<"KEY-ID">>)
```

### update
Update the specified key's information.
> [https://m2x.att.com/developer/documentation/v2/keys#Update-Key](https://m2x.att.com/developer/documentation/v2/keys#Update-Key)

```erlang
m2x_key:update(Client, <<"KEY-ID">>, Params)
```

### delete
Delete the specified key.
> [https://m2x.att.com/developer/documentation/v2/keys#Delete-Key](https://m2x.att.com/developer/documentation/v2/keys#Delete-Key)

```erlang
m2x_key:delete(Client, <<"KEY-ID">>)
```

### regenerate
Regenerate the specified key.
> [https://m2x.att.com/developer/documentation/v2/keys#Regenerate-Key](https://m2x.att.com/developer/documentation/v2/keys#Regenerate-Key)

```erlang
m2x_key:regenerate(Client, <<"KEY-ID">>)
```
