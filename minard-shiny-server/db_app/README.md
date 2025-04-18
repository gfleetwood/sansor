# Database RU Prototype

There's a constant bifurcation between the technical and non-technical interaction with data, especially with databases. While developers work directly with the database via SQL, others are more comfortable using GUI based methods like spreadsheets. By default this excludes the notion of a single source of truth for data. Since a database is the obvious candidate for this source of truth, the goal is to bring non-developers into the sphere of interacting with the database without having to learn SQL. 

There are a number of products that (try to) solve this problem. The number of free solutions are an order of magnitude lower than paid ones, and I didn't gravitate to any of them. This prototype uses Shiny as my attempt at another. 

On the backend a database connection is harded coded. The user can then choose from a list of tables which they can edit and push the changes back to the database. (Hence "RU" instead of "CRUD".) The scaffolding assumes the existence of a change log table:

```
tbl1 = Table(
'TABLE_change_log',
metadata,
Column("row", Integer, nullable = False), 
Column("col", Integer, nullable = False),
Column("original_value", String(500), nullable = False),
Column("updated_value", String(500), nullable = False),
Column("created", DateTime, nullable = False)
)
```

This logs every change made to the table so a rollback can be conducted. The change log is updated in the database at the same time as the table.

The main code can be found in `app.R` and `app_technical.R`.
