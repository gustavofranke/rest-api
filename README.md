# rest-api

After having seen [Your First Web Application with Spock](https://www.youtube.com/watch?v=Orm-jIIgVD0) on YouTube (and having played with the code a little bit), I wanted to explore this nice library, it was then when I found some [tutorials on their website](https://www.spock.li/tutorials/).

This repository is the result of melding them all into one project. As a result, we have woking REST API that exposes a few endpoints around one entity, that is persisted to a local sqlite instance.

Unfortunately, after that I found [this in their issue tracker](https://github.com/agrafix/Spock/issues/164) suggesting that the project is no longer actively maintained.

These are a few `curl` commands that can be run agains the API,
```
curl -H "Content-Type: application/json" -d '{ "name": "Bart", "age": 10 }' localhost:8080/people | jq '.'
curl -H "Content-Type: application/json" -d '{ "name": "Walter", "age": 50 }' localhost:8080/people | jq '.'
curl -H "Content-Type: application/json" -d '{ "name": "Jesse", "age": 22 }' localhost:8080/people | jq '.'
curl localhost:8080/people | jq '.'
curl localhost:8080/people/1 | jq '.'
curl -X PUT -H "Content-Type: application/json" -d '{ "name": "qwerq", "age": 14 }' localhost:8080/people/7/ | jq '.'
curl -X DELETE localhost:8080/people/7 | jq '.'
-}
```

To star the app, run `stack exec rest-api`, you should see output like:
```
Spock is running on port 8080
```

Then, for instance; run, `curl localhost:8080/people/1 | jq '.'`, you should see output like:
```
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100    24    0    24    0     0   1090      0 --:--:-- --:--:-- --:--:--  1090
{
  "age": 10,
  "name": "Bart"
}
```