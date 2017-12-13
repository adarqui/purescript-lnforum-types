build:
	pulp -w build

id:
	pscid -p 4289

build-psa:
	pulp -w build --stash --censor-lib --censor-codes=ImplicitImport,UnusedExplicitImport,HidingImport,ImplicitQualifiedImport

tests:
	pulp -w test

bower:
	bower install
	rsync -av ../purescript-foreign-extra/src/ ./bower_components/purescript-foreign/src/

curtag:
	git push origin :v0.11.0
	git push origin v0.11.0
