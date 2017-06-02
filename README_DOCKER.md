# Running SnoLyze in a Docker container

The container needs access to the SNOMED CT relationship snapshot file to initialize SnoLyze.

Run the following commands in a command prompt:

```shell
docker build -t snolyze .

docker run -d -p <your preferred port>:8787 -v <directory with snomed files>:/home/rstudio/snomed snolyze
```


