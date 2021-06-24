unit observerTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ComCtrls, MaskEdit, Contnrs, typinfo;

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

  { TWatchedClass }
  TWatchedClass = class
  private
    fVariable: integer;
    fSubject: TMySubject;
  public
    constructor Create;
    destructor Destroy; override;
    procedure setVariable(variable: integer);
  published
    property subject: TMySubject read fSubject;
    property variable: integer read fVariable write setVariable;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    bAddObserver: TButton;
    Button1: TButton;
    eObserver: TEdit;
    lbLog: TListBox;
    me1: TMaskEdit;
    UpDown1: TUpDown;
    procedure bAddObserverClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure UpDown1Click(Sender: TObject; Button: TUDBtnType);
  private

  public

  end;

var
  Form1: TForm1;
  watchedObject: TWatchedClass;
implementation

{$R *.lfm}

{ TWatchedClass }

constructor TWatchedClass.Create;
begin
  inherited Create;
  fSubject:=TMySubject.Create(self);
end;

destructor TWatchedClass.Destroy;
begin
  fSubject.Free;
  inherited Destroy;
end;

procedure TWatchedClass.setVariable(variable: integer);
begin
  fVariable:= variable;
  fSubject.Notify;
end;



{ TForm1 }

procedure TForm1.FormShow(Sender: TObject);
begin
  watchedObject:=TWatchedClass.Create;
  lbLog.items.Add('Watched object created with no observers');
end;

procedure TForm1.UpDown1Click(Sender: TObject; Button: TUDBtnType);
begin
  me1.Text:=inttostr(upDown1.Position);
end;

procedure TForm1.bAddObserverClick(Sender: TObject);
var
  newObserver:TMyObserver;
begin
  newObserver:=TMyObserver.Create;
  newObserver.name:=eObserver.text;
  Form1.lbLog.items.add('Attaching observer '+ newObserver.name);
  watchedObject.subject.Attach(newObserver);
  Form1.lbLog.items.add('Observer count is now '+inttostr(watchedObject.subject.Observers.Count));
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  observers: TObjectList;
  observer: TMyObserver;
begin
  Form1.lbLog.items.add('Remove observer');
  if (watchedObject <> nil) and (watchedObject.subject.Observers <> nil) then
    begin
    observers:=watchedObject.subject.Observers;
    if (observers <> nil) and (observers.Count > 0) then
      begin
        observer:=observers[0] as TMyObserver;
        watchedObject.subject.Detach(observer);
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
data: integer;
dataPtr: pointer;
begin
data:=1;
dataPtr:=@data;
if fObservers <> nil then
for i := 0 to Pred(fObservers.Count) do
TMyObserver(fObservers[i]).FPOObservedChanged(fController, TFPObservedOperation.ooChange, dataPtr);
end;


end.

