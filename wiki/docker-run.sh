#!/bin/bash

sudo docker run --rm --name smw -p 8081:80 -v $(pwd)/data:/var/www/html/data/ smw
