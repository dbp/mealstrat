# A meal planner

## Setup

Create database:

```
$ psql template1
template1# CREATE DATABASE mealstrat_devel;
template1# CREATE USER mealstrat_user WITH PASSWORD '111';
template1# GRANT ALL ON DATABASE mealstrat_devel TO mealstrat_user;
```

Load schema:

```
psql -U mealstrat_user -W -h localhost mealstrat_devel < migrations/base.sql
Password for user mealstrat_user: [Enter 111]
```

Build:

```
stack build
```

Run:

```
stack exec mealstrat
```
