unit iobserver_interface;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type
IObserver = interface
  ['{a4350679-517f-4c72-b3f8-5cf7abfdf2be}']
    procedure Update(Subject: IInterface);
  end;
implementation

end.

