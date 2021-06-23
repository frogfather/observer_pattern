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
  published
    property Observers: TObjectList read fObservers;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    bAddObserver: TButton;
    bNotify: TButton;
    Button1: TButton;
    eObserver: TEdit;
    lbLog: TListBox;
    procedure bAddObserverClick(Sender: TObject);
    procedure bNotifyClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  Subject: TMySubject;
implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  Subject:=TMySubject.Create(self);
  lbLog.items.Add('Subject created with no observers');
end;

procedure TForm1.bAddObserverClick(Sender: TObject);
var
  newObserver:TMyObserver;
begin
  newObserver:=TMyObserver.Create;
  newObserver.name:=eObserver.text;
  Form1.lbLog.items.add('Attaching observer '+ newObserver.name);
  Subject.Attach(newObserver);
  Form1.lbLog.items.add('Observer count is now '+inttostr(Subject.Observers.Count));
end;

procedure TForm1.bNotifyClick(Sender: TObject);
begin
  Subject.Notify;
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  observers: TObjectList;
  observer: TMyObserver;
begin
  Form1.lbLog.items.add('Remove observer');
  if (subject <> nil) and (subject.Observers <> nil) then
    begin
    observers:=subject.Observers;
    if (observers <> nil) and (observers.Count > 0) then
      begin
        observer:=observers[0] as TMyObserver;
        subject.Detach(observer);
      end;
    end;
end;


{ TMyObserver }

procedure TMyObserver.FPOObservedChanged(ASender: TObject;
  Operation: TFPObservedOperation; Data: Pointer);

function OperationToString(AOperation: TFPObservedOperation): string;
  begin
    result := GetEnumName(TypeInfo(TFPObservedOperation), Ord(AOperation));
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
    Form1.lbLog.items.add('There are '+inttostr(fObservers.Count)+' observers ');
    fObservers.Remove(Observer);
    if fObservers.Count = 0 then
    begin
      Form1.lbLog.items.add('Freeing observer list');
      fObservers.Free;
      fObservers := nil;
    end;
  end;
end;


procedure TMySubject.Notify;
var
i: Integer;
dataPtr: ^string;
data: String;
begin
data:='Random data';
dataPtr:= @data;
if fObservers <> nil then
for i := 0 to Pred(fObservers.Count) do
TMyObserver(fObservers[i]).FPOObservedChanged(fController, TFPObservedOperation.ooChange, dataPtr);
end;


end.

