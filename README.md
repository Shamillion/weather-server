# weather-server

## Project deployment

To deploy the project, you need to perform the following steps:
1. Clone this repository.
2. Open terminal and go to the root folder of the project.
3. Compile the project with the 
   ```haskell
   cabal build
   ```
   command.
4. Make a copy of the **_config.json** file in the root folder of the project and rename this copy to **config.json**.    
5. Open file **config.json** in root folder of the project and fill up your values
6. Run the server with the 
   ```haskell
   cabal run
   ```
   command from the root folder of the project.
7. Open one more terminal to send requests to the server or use browser.


## Examples of requests to the server

### Requests without a timestamp

```
curl -X GET 'http://localhost:8080/London'
```
```
curl -X GET 'http://localhost:8080/London,GB'
```
```
curl -X GET 'http://localhost:8080/London,uk'
```

```
curl -X GET 'http://localhost:8080/Vladivostok'
```
```
curl -X GET 'http://localhost:8080/Vladivostok,RU'
```

### Timestamp requests
```
curl -X GET 'http://localhost:8080/London?timestamp=1723385867'
```
```
curl -X GET 'http://localhost:8080/London,GB?timestamp=1723385867'
```
```
curl -X GET 'http://localhost:8080/Vladivostok?timestamp=1723373592'
```
```
curl -X GET 'http://localhost:8080/Vladivostok,ru?timestamp=1723373592'
```