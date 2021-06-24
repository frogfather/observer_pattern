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
  published
    property Observers: TObjectList read fObservers;
  end;

  { TWatchedClass }
  //A class that has a TMySubject field
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

  { TObservingClass }
  //Implements the abstract update method
  TObservingClass = class(TMyObserver)
  private
    procedure Update(const Subject: TObject) override;
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

{ TObservingClass }

procedure TObservingClass.Update(const Subject: TObject);
var
  vInt:integer;
  pVInt: ^integer;
begin
  if subject is TWatchedClass then with subject as TWatchedClass do
    begin
      form1.lbLog.items.add('Value changed to '+inttostr(variable));
      //get the variable and a pointer to it
      vInt := variable;
      pVInt := @vInt;
      FPOObservedChanged(subject, TFPObservedOperation.ooChange, pVint);
    end;
end;

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
  watchedObject.setVariable(upDown1.Position);
end;

procedure TForm1.bAddObserverClick(Sender: TObject);
var
  newObserver:TObservingClass;
begin
  newObserver:=TObservingClass.Create;
  newObserver.name:=eObserver.text;
  Form1.lbLog.items.add('Attaching observer '+ newObserver.name);
  watchedObject.subject.Attach(newObserver);
  Form1.lbLog.items.add('Observer count is now '+inttostr(watchedObject.subject.Observers.Count));
end;


procedure TForm1.Button1Click(Sender: TObject);
var
  observers: TObjectList;
  observer: TObservingClass;
begin
  Form1.lbLog.items.add('Remove observer');
  if (watchedObject <> nil) and (watchedObject.subject.Observers <> nil) then
    begin
    observers:=watchedObject.subject.Observers;
    if (observers <> nil) and (observers.Count > 0) then
      begin
        observer:=observers[0] as TObservingClass;
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
    fObservers.Remove(Observer);
    Form1.lbLog.items.add('There are '+inttostr(fObservers.Count)+' observers ');
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
begin
if fObservers <> nil then
for i := 0 to Pred(fObservers.Count) do
TMyObserver(fObservers[i]).Update(fController);
end;


end.

