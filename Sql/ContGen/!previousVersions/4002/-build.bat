md .\!All
del .\!All\all.sql

copy /b .\tables\*.sql + .\functions\*.sql .\!All\all.sql
