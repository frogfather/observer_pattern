unit observerTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, Contnrs, typinfo;

type

  { TMyObserver }
  TMyObserver = class(TObject, IFPObserver)
  private
    fName: string;
    procedure Update(const Subject: TObject); virtual; abstract;
    procedure FPOObservedChanged(ASender : TObject; Operation : TFPObservedOperation; Data : Pointer);
  published
    property name: string read fName write fName;
  end;

  { TMySubject }
  TMySubject = class
  private
  fController: TObject;
  fObservers: TObjectList;
  public
  constructor Create(const Controller: TObject);
  procedure Attach(const Observer: TMyObserver);
  procedure Detach(const Observer: TMyObserver);
  procedure Notify;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    bAdd: TButton;
    bAddObserver: TButton;
    bRemoveObserver: TButton;
    eItem: TEdit;
    lbObserved: TListBox;
    lbLog: TListBox;
  private

  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TMyObserver }

procedure TMyObserver.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);

function OperationToString(AOperation: TFPObservedOperation): string;
  begin
    result := GetEnumName(TypeInfo(TFPObservedOperation),
                         Ord(AOperation));
  end;

var
  intf: IFPObserved;
begin
  if Operation = ooFree then
  begin
    Form1.lbLog.items.add('[ooFree] detected so we should detach ourselves');
    if Supports(ASender, IFPObserved, intf) then
      intf.FPODetachObserver(self);
  end
  else
  begin
    Form1.lbLog.items.add(self.name+': '+ASender.ClassName + ' has changed ['+ OperationToString(Operation) + ']');
  end;
end;

{ TMySubject }

constructor TMySubject.Create(const Controller: TObject);
begin
  inherited Create;
    fController := Controller;
end;

procedure TMySubject.Attach(const Observer: TMyObserver);
begin
if fObservers = nil then
fObservers := TObjectList.Create;
if fObservers.IndexOf(Observer) < 0 then
fObservers.Add(Observer);
end;

procedure TMySubject.Detach(const Observer: TMyObserver);
begin
if fObservers <> nil then
begin
fObservers.Remove(Observer);
if fObservers.Count = 0 then
begin
fObservers.Free;
fObservers := nil;
end;
end;
end;

procedure TMySubject.Notify;
var
i: Integer;
begin
if fObservers <> nil then
for i := 0 to Pred(fObservers.Count) do
TMyObserver(fObservers[i]).Update(fController);
end;

{ TForm1 }

end.

