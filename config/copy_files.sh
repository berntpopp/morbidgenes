#!/bin/bash
cp config/api/config.yml morbidgenes/api/
cp config/app/.env.production morbidgenes/app/
cp config/nginx/* morbidgenes/app/docker/nginx/
cp config/docker-compose.sh morbidgenes/
cp config/docker/.env morbidgenes/