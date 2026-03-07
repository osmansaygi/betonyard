# STA4CAD -> AutoCAD Importer

Bu proje, `Sta4CadImporterAutoCadCommand.cs` icindeki komutu AutoCAD eklentisi olarak derlemek icindir.

## 1) Gereksinimler

- AutoCAD (x64)
- .NET 8 SDK
- Visual Studio 2022 (veya `dotnet` CLI)

## 2) DLL referans yolunu kontrol et

`BetonLisp.AutoCAD.csproj` icindeki su satiri kontrol et:

- `AutoCADInstallDir`

Varsayilan:

- `C:\Program Files\Autodesk\AutoCAD 2025`

Farkli surum kullaniyorsan kendi kurulum yoluna guncelle.

## 3) Derleme

Visual Studio ile:

- `BetonLisp.AutoCAD.csproj` dosyasini ac
- `Release | x64` sec
- Build al

MSBuild ile:

```powershell
dotnet build .\BetonLisp.AutoCAD.csproj -c Release
```

Uretilen DLL:

- `bin\Release\net8.0-windows\BetonLisp.AutoCAD.dll`

## 4) AutoCAD icinde yukleme

1. AutoCAD'de `NETLOAD` komutunu calistir
2. `BetonLisp.AutoCAD.dll` dosyasini sec
3. Komutu calistir: `ImportSTA4CAD`
4. `.st4` dosyasini sec

## 5) Notlar

- Komut, su katmanlari otomatik olusturur/temizler:
  - `KOLONLAR`
  - `KIRISLER`
  - `DOSEMELER`
  - `AKSLAR`
  - `ETIKETLER`
- Cizim bittiginde `ZOOM EXTENTS` uygulanir.
