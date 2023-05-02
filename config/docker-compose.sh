#!/bin/bash
docker-compose --env-file .env up -d --build --scale api=2