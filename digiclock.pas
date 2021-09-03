unit DigiClock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  IObserver = interface
  ['{a4350679-517f-4c72-b3f8-5cf7abfdf2be}']
    procedure Update(Subject: IInterface);
  end;

  { DigiClock }

  { DigitalClock }

  DigitalClock = class(TPanel, IObserver)
  private
    procedure ObserverUpdate(const Subject: IInterface);
  protected

  public

  published

  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Misc',[DigiClock]);
end;

{ DigitalClock }

procedure DigitalClock.ObserverUpdate(const Subject: IInterface);
var
Obj: IClockTimer;
begin
Subject.QueryInterface(IClockTimer, Obj);
if Obj <> nil then
Caption := FormatDateTime(‘tt’, Obj.GetTime);
end;

end.
