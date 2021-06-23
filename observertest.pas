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

  { TSubject }
  TSubject = class
  private
  fController: TObject;
  fObservers: TObjectList;
  public
  constructor Create(const Controller: TObject);
  procedure Attach(const Observer: TMyObserver);
  procedure Detach(const Observer: TMyObserver);
  procedure Notify;
  end;

  { TObserverList }
  TObserverList = array of TMyObserver;

  { TForm1 }

  TForm1 = class(TForm)
    bAdd: TButton;
    bAddObserver: TButton;
    bRemoveObserver: TButton;
    eItem: TEdit;
    lbObserved: TListBox;
    lbLog: TListBox;
    Timer1: TTimer;
    procedure bAddClick(Sender: TObject);
    procedure bAddObserverClick(Sender: TObject);
    procedure bRemoveObserverClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  ObserverList: TObserverlist;

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

{ TSubject }

constructor TSubject.Create(const Controller: TObject);
begin
  inherited Create;
    fController := Controller;
end;

procedure TSubject.Attach(const Observer: TMyObserver);
begin
if fObservers = nil then
fObservers := TObjectList.Create;
if fObservers.IndexOf(Observer) < 0 then
fObservers.Add(Observer);
end;

procedure TSubject.Detach(const Observer: TMyObserver);
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

procedure TSubject.Notify;
var
i: Integer;
begin
if fObservers <> nil then
for i := 0 to Pred(fObservers.Count) do
TMyObserver(fObservers[i]).Update(fController);
end;

{ TForm1 }

procedure TForm1.bAddClick(Sender: TObject);
begin
  if (lbObserved.items.indexOf(eItem.text) = -1) then
  begin
  lbObserved.items.add(eItem.text);
  eItem.Clear;
  end;

end;

procedure TForm1.bAddObserverClick(Sender: TObject);
var
  oObserver: TMyObserver;
  intf: IFPObserved;
begin
 oObserver:=TMyObserver.Create;
 oObserver.name:='Observer '+inttostr(length(observerlist)+1);
 { attach observer }
  if Supports(lbObserved.items, IFPObserved, intf) then
  begin
    lbLog.items.add('adding observer '+oObserver.name);
    intf.FPOAttachObserver(oObserver);
    setLength(ObserverList, length(ObserverList) + 1);
    ObserverList[length(ObserverList)-1] := oObserver;
    lbLog.Items.Add('There are now '+inttostr(length(observerlist))+' observers ');
  end else
  begin
    lbLog.items.add(lbObserved.ClassName+' does not support IFPObserved');
  end;


end;

procedure TForm1.bRemoveObserverClick(Sender: TObject);
var
  oObserver: TMyObserver;
begin
  if (length(ObserverList) > 0 ) then
    begin
      oObserver:=ObserverList[length(ObserverList)-1];
      lbLog.items.add('removing observer '+oObserver.name);
      lbObserved.items.FPODetachObserver(oObserver);
      setLength(ObserverList, length(ObserverList) - 1);
      lbLog.Items.Add('There are now '+inttostr(length(observerlist))+' observers ');
    end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  setLength(ObserverList, 0);
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  setLength(ObserverList, 0);
end;

end.

