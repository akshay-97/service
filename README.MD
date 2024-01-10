Instructions to start server:
cabal run


## sample curl
curl --location --request POST 'http://localhost:8081/convert' \
--header 'Content-Type: application/json' \
--data-raw '{"req": "#00date|1997-02-06#02name|bob#01age|20#03hasPassport|Y#12access|read_db,write_db,view_logs"}'