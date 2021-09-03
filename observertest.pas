unit observerTest;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  ExtCtrls, ComCtrls, MaskEdit, Contnrs, typinfo;

type

  IObserver = interface
  ['{a4350679-517f-4c72-b3f8-5cf7abfdf2be}']
    procedure Update(Subject: IInterface);
  end;

  ISubject = interface
  ['{18dbf567-b454-418e-b3dd-d076eb395837}']
  procedure Attach(Observer: IObserver);
  procedure Detach(Observer: IObserver);
  procedure Notify;
  function ObserverCount: integer;
  function GetObserverByName(name: string): IObserver;
  end;

  IClockTimer = interface
  ['{826ea8f8-133f-4d6d-a03d-aa055fe2dff7}']
  function GetTime: TDateTime;
  end;

  { TSubject }

  TSubject = class(TInterfacedObject, ISubject)
  private
    fController: Pointer;
    fObservers: IInterfaceList;
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify;
  public
    function ObserverCount: integer;
    function getObserverByName(name: string): IObserver;
    constructor Create(const Controller: IInterface);
  end;

  TClockTimer = class(TInterfacedObject, IClockTimer, ISubject)
  private
    fTimer: TTimer;
    fInternalTime: TDateTime;
    fSubject: ISubject;
    function GetTime: TDateTime;
    procedure Tick(Sender: TObject);
    property Subject: ISubject read fSubject implements ISubject;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TMyObserver }
  TMyObserver = class(TInterfacedObject, IObserver)
    private
      fName: string;
    procedure Update(subject: IInterface);
    public
    constructor create(name:String);
    destructor destroy; override;
    property oName: string read fName;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    bAdd: TButton;
    bRemove: TButton;
    eObserverName: TEdit;
    lbLog: TListBox;
    procedure bAddClick(Sender: TObject);
    procedure bRemoveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private

  public

  end;

var
  Form1: TForm1;
  fClockTimer: TClockTimer;
implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
    fClockTimer := TClockTimer.Create;
    fClockTimer._AddRef;
end;

procedure TForm1.bAddClick(Sender: TObject);
var
  newObserver:IObserver;
  observerCount:integer;
begin
  observerCount := fClockTimer.Subject.ObserverCount;
  newObserver:=TMyObserver.create('Obs '+intToStr(observerCount + 1));
  fClockTimer.Subject.Attach(newObserver);
end;

procedure TForm1.bRemoveClick(Sender: TObject);
var
  selected: IObserver;
begin
  //remove observer with name specified
  selected:=fClockTimer.Subject.GetObserverByName(eObserverName.Text);
  if (selected <> nil) and (selected is TMyObserver) then
    begin
    fClockTimer.Subject.Detach(selected);
    end;
end;

{ TMyObserver }

procedure TMyObserver.Update(subject: IInterface);
var
Obj: IClockTimer;
begin
Subject.QueryInterface(IClockTimer, Obj);
if Obj <> nil then
Form1.lbLog.items.add('Observer '+oName+': '+FormatDateTime('tt', Obj.GetTime));
Form1.lbLog.Selected[Form1.lbLog.Items.Count - 1]:= true;
end;

constructor TMyObserver.create(name: string);
begin
 fName:=name;
end;

destructor TMyObserver.destroy;
begin

end;

{ TSubject }

constructor TSubject.Create(const Controller: IInterface);
begin
  form1.lbLog.items.add('Create instance of TSubject');
  inherited Create;
  fController := Pointer(Controller);
end;

procedure TSubject.Attach(Observer: IObserver);
begin
form1.lbLog.items.add('TSubject.attach called');
if fObservers = nil then
fObservers := TInterfaceList.Create;
fObservers.Add(Observer);
end;

procedure TSubject.Detach(Observer: IObserver);
begin
form1.lbLog.items.add('TSubject.detach called');
if fObservers <> nil then
  begin
  fObservers.Remove(Observer);
  if fObservers.Count = 0 then
  fObservers := nil;
  end;
end;

procedure TSubject.Notify;
var
i: Integer;
begin
  if fObservers <> nil then
  for i := 0 to Pred(fObservers.Count) do
  (fObservers[i] as IObserver).Update(IInterface (fController));
end;

function TSubject.ObserverCount: integer;
begin
  if (fObservers = nil) then result:=0 else result := fObservers.Count;
end;

function TSubject.getObserverByName(name: string): IObserver;
var
i:integer;
begin
  if (fObservers <> nil) and (fObservers.Count > 0) then for i:= 0 to Pred(fObservers.Count) do
    begin
    if (fObservers[i] as TMyObserver).oName = name then
      begin
      result:=fObservers[i] as TMyObserver;
      exit;
      end;
    end;
  result := nil;
end;

{ TClockTimer }

procedure TClockTimer.Tick(Sender: TObject);
begin
  fInternalTime := Now;
  fSubject.Notify;
end;

constructor TClockTimer.Create;
begin
  form1.lbLog.items.add('Create instance of TClock timer');
  inherited create;
  fSubject:=TSubject.Create(self);
  fTimer:=TTimer.Create(nil);
  //We set the OnTimer event of fTimer to a pointer to the tick method
  fTimer.OnTimer:=@Tick;
  fTimer.Enabled:=true;
end;

destructor TClockTimer.Destroy;
begin
  form1.lbLog.items.add('destroy TClockTimer');
  fTimer.Enabled := False;
  fTimer.Free;
    inherited Destroy;
end;

function TClockTimer.GetTime: TDateTime;
begin
  Result := fInternalTime;
end;



end.

