Card list can be generated using 
```bash
jq '[.data[].cards[] | select ( .legalities.pioneer == "Legal" ) | select ((.side // "a") == "a") | .faceName // .name] | unique
```
