# DynamoSQL

A very early PoC which will, in all likelihood, never become more than that as I'll probably never have the time to either complete it or support it. But the idea is that it'll let you write DynamoDB queries like this:

```scala
val query = query"""
  SELECT *
  FROM MyTable INDEX MyIndex
  WHERE MyPK = $foo AND MySK = $bar
  FILTER SomeAttr BETWEEN $min AND $max
  LIMIT $limit
"""
```

With different interpolators for the other operations including updates.