csv2sql
======
Instalation
-----------
$ghc --make setup
$./setup configure --prefix=$HOME --user
$./setup build
$./setup install
Runing
------
$./csv2sql -I path/to/csv/dir -O path/to/sql/dir

"path/to/csv/dir" is a path to directory which contain *.csv files (else it can be a singl file)
"path/to/sql/dir" is a path to directory which will contain *.sql files (it mast be path to sigle file (*.sql) if "path/to/csv/dir" is a a path to sigle file (*.csv)) 