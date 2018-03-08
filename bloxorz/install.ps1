dotnet new -i Fable.Template
dotnet new fable -lang "F#"
rm -force -r .paket
iwr http://bit.ly/2I9pVAO -o paket.cmd
yarn install
./paket install
