# Running SnoLyze in a Docker container

```shell
docker build -t snolyze .

docker run -d -p <your preferred port>:8787 -v <directory with snomed files>:/home/rstudio/snomed snolyze
```

