# How to use this image

## Bot credentials

Create a file named 'credentials.R' in the "shiny/shiny-server" directory containing the instantiation of the user and password variables, for example:

```R
username <- 'ForAPI'
password <- '8dV6VfMjgFsrmg'
```


## Database parameters

Fix the username and password for the database in the YAML file; this will be required in MediaWiki setup. Take note also of the database server name if you change it.


## Start the containers

For starting this set of containers you must use Docker Compose (please check that for the first invocation the line for copying the file **LocalSettings.php** is commented; this directive should be located at line 20 of the YAML file):

```shell
$ sudo docker-compose up
```

Then, access it via http://localhost:8081/mw-config/ in a browser.


## MediaWiki configuration

When accessing the web server for the first time, a brief setup process will be started. Please remember to change the Database host to 'database'. In order to enforce user roles you have to opt for a private wiki. For security reasons there is not an email server in the Docker images.

Once done,

1. save the LocalSettings.php file in the same directory of the Docker-Compose configuration,

2. uncomment the relevant line in the YAML file (should be line 20),

3. append at the end of the file the following line
```php
   include_once "LocalSettings.local.php";
``` 

4. and finally restart the images.


## Access the web server

Before to access to the web server at http://localhost:8081/ update it with the command

```shell
sudo docker-compose exec smw php maintenance/update.php --quick
```


## Completing bot setup

Create an user for accessing APIs having the previous credentials set for **ForAPI** (from the [Create user](http://localhost:8081/index.php/Special:CreateAccount) special page) and give it the **bot**, **viewer**, and **export** privileges (from the [User rights](http://localhost:8081/index.php/Special:UserRights) special page).



## Import data

Finally, import the XML files with `maintenance/importDump.php` and update as usual:

```shell
$ sudo docker-compose exec smw php maintenance/importDump.php schema_Virus.xml
$ sudo docker-compose exec smw php maintenance/importDump.php data_Virus_pos.xml
$ sudo docker-compose exec smw php extensions/SemanticMediaWiki/maintenance/rebuildData.php
$ sudo docker-compose exec smw php maintenance/update.php --quick
$ sudo docker-compose exec smw php maintenance/runJobs.php
```

Please ensure that the job queue is empty.

*Nota bene:*  to be able to convert Semantic Drilldown filters into ask queries you mus belong to the **exporters** group.