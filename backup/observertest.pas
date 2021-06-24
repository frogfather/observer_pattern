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
  end;

  IClockTimer = interface
  ['{826ea8f8-133f-4d6d-a03d-aa055fe2dff7}']
  function GetTime: TDateTime;
  end;

  TSubject = class(TInterfacedObject, ISubject)
  private
    fController: Pointer;
    fObservers: IInterfaceList;
    procedure Attach(Observer: IObserver);
    procedure Detach(Observer: IObserver);
    procedure Notify;
  public
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
    procedure Update(subject: IInterface);
    public
    constructor create;
    destructor destroy; override;
  end;

  { TForm1 }

  TForm1 = class(TForm)
    bAdd: TButton;
    bRemove: TButton;
    lbLog: TListBox;
    procedure bAddClick(Sender: TObject);
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
begin
  newObserver:=TMyObserver.create;
  fClockTimer.Subject.Attach(newObserver);
end;

{ TMyObserver }

procedure TMyObserver.Update(subject: IInterface);
var
Obj: IClockTimer;
begin
Subject.QueryInterface(IClockTimer, Obj);
if Obj <> nil then
Form1.lbLog.items.add(FormatDateTime('tt', Obj.GetTime));
end;

constructor TMyObserver.create;
begin

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
  form1.lbLog.items.add('TSubject.notify called');
  if fObservers <> nil then
  for i := 0 to Pred(fObservers.Count) do
  (fObservers[i] as IObserver).Update(IInterface (fController));
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

