{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit digital_clock;

{$warn 5023 off : no warning about unused units}
interface

uses
  DigiClock, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('DigiClock', @DigiClock.Register);
end;

initialization
  RegisterPackage('digital_clock', @Register);
end.
