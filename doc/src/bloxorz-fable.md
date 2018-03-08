Fable quickstart
----

## Install

* create and run `install.ps1`
```powershell
dotnet new -i Fable.Template
dotnet new fable -lang "F#"
rm -force -r .paket
iwr http://bit.ly/2I9pVAO -o paket.cmd
yarn install
./paket install
```

* edit `package.json`
```json
  "scripts": {
    //...
    "fable": "dotnet restore && cd src && dotnet fable yarn-start"
    //...
  },
```

* execute `yarn run fable` (and forget)

