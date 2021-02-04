unit mainunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls, ExtCtrls, Menus, EditBtn, IDEWindowIntf, math;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    CheckBox1: TCheckBox;
    ComboBox1: TComboBox;
    ComboBox2: TComboBox;
    Edit1: TEdit;
    Edit3: TEdit;
    Edit5: TEdit;
    FileNameEdit1: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MainMenu1: TMainMenu;
    Memo1: TMemo;
    Memo2: TMemo;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    OpenDialog1: TOpenDialog;
    PageControl1: TPageControl;
    Panel1: TPanel;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button6Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Memo2KeyPress(Sender: TObject; var Key: char);
    procedure MenuItem2Click(Sender: TObject);
  private
    fSerialThread: TThread;
    procedure DoProgram;
    { private declarations }
  public
    fBuffer: string;
    fBufferMutex: TRTLCriticalSection;
    procedure GotData;
    { public declarations }
  end; 

var
  Form1: TForm1; 

implementation

uses elfobj, synaser, comports;

type

 { TBlockSerialThread }

 TBlockSerialThread = class(TThread)
 private
  fInputCS: TRTLCriticalSection;
  fInput: string;
  function PullInput: string;
 protected
  procedure Execute; override;
 public
  procedure AddInput(const s: string);

  constructor Create(CreateSuspended: boolean);
  destructor Destroy; override;
 end;

{ TBlockSerialThread }

function TBlockSerialThread.PullInput: string;
begin
   EnterCriticalsection(fInputCS);
   result := fInput;
   fInput := '';
   LeaveCriticalsection(fInputCS);
end;

procedure TBlockSerialThread.Execute;
var ser: TBlockSerial;
    buf: array[0..1023] of char;
    res: longint;
    h: string;
begin
   ser := TBlockSerial.Create;
   try
      ser.Connect(form1.ComboBox2.Text);
      ser.Config(strtoint(Form1.Edit5.text), 8, 'N', SB1, false,false);

      ser.Purge;

      while not Terminated do
      begin
         FillChar(buf[0], sizeof(buf), 0);
         res := ser.RecvBufferEx(@buf[0],length(buf),100);

         if res <= 0 then continue;

         EnterCriticalsection(form1.fBufferMutex);
         form1.fBuffer := form1.fBuffer+buf;
         LeaveCriticalsection(form1.fBufferMutex);

         Synchronize(@Form1.GotData);

         h := PullInput;
         if length(h) > 0 then
         begin
            ser.SendBuffer(@h[1], length(h));
         end;
      end;
   finally
      ser.free;
   end;
end;

procedure TBlockSerialThread.AddInput(const s: string);
begin
   EnterCriticalsection(fInputCS);
   fInput := fInput+s;
   LeaveCriticalsection(fInputCS);
end;

constructor TBlockSerialThread.Create(CreateSuspended: boolean);
begin
   inherited Create(CreateSuspended);
   InitCriticalSection(fInputCS);
end;

destructor TBlockSerialThread.Destroy;
begin
   DoneCriticalsection(fInputCS);
   inherited Destroy;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
   if OpenDialog1.Execute then
      edit1.text := OpenDialog1.FileName;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
   if not FileExists(edit1.text) then
   begin
      ShowMessage('File does not exist');
      exit;
   end;

   try
      DoProgram;
   except
      on e: exception do
         showmessage('Error: '+e.message);
   end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
   InitCriticalSection(fBufferMutex);
   fSerialThread := TBlockSerialThread.Create(false);
   button3.Enabled := false;
   button4.Enabled := true;
   button5.Enabled := false;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
   fSerialThread.Terminate;
   fSerialThread.WaitFor;
   fSerialThread.Free;
   fSerialThread := nil;
   DoneCriticalsection(fBufferMutex);
   button3.Enabled := true;
   button4.Enabled := false;
   button5.Enabled := true;
end;

procedure TForm1.Button5Click(Sender: TObject);
var ser: TBlockSerial;
begin
   ser := TBlockSerial.Create;
   try
      ser.Connect(form1.ComboBox2.Text);
      ser.Config(strtoint(Form1.Edit5.text), 8, 'N', SB1, false,false);

      ser.Purge;
   finally
      ser.free;
   end;
end;

procedure TForm1.Button6Click(Sender: TObject);
begin
   Memo2.lines.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   ComboBox1.Items.Clear;
   ComboBox2.Items.Clear;
   DetectComPorts(ComboBox1.Items);
   DetectComPorts(ComboBox2.Items);
end;

procedure TForm1.Memo2KeyPress(Sender: TObject; var Key: char);
begin
   if fSerialThread <> nil then
      TBlockSerialThread(fSerialThread).AddInput(key);
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
   ComboBox1.Items.Clear;
   ComboBox2.Items.Clear;
   DetectComPorts(ComboBox1.Items);
   DetectComPorts(ComboBox2.Items);
end;

procedure TForm1.DoProgram;
type
 TCommands = array[0..10] of byte;
var ser: TBlockSerial;
    commands: TCommands;
    version: byte;
    id: word;
    i,i2,sz,addr,rd: longint;
    fs: TStream;
    elf: TElfObject;
    buffer,buffer2: array[0..63] of longword;
const
 Timeout = 3000;

 ACK = $79;
 NACK = $1F;

 CMD_Get = $00;
 CMD_GetVersion = $01;       CO_GetVersion = 1;
 CMD_GetID = $02;            CO_GetID = 2;
 CMD_ReadMem = $11;          CO_ReadMem = 3;
 CMD_Go = $21;
 CMD_WriteMem = $31;         CO_WriteMem = 5;
 CMD_Erase = $43;            CO_Erase = 6;
 CMD_ExtendedErase = $44;
 CMD_WriteProtect = $63;
 CMD_WriteUnprotect = $73;
 CMD_ReadoutProtect = $82;
 CMD_ReadoutUnprotect = $92;

   function WaitforACK: boolean;
   var b: Byte;
   begin
      b := ser.RecvByte(Timeout);
      result := b = ACK;
   end;

   procedure Initialize;
   begin
      ser.SendByte($7F);
      if not WaitforACK then raise exception.Create('Did not receive proper acknowledge while initializing');
   end;

   function SendCommand(Cmd: byte): boolean;
   begin
      ser.SendByte(cmd);
      ser.SendByte(cmd XOR $FF);
      result := WaitforACK;
   end;

   function Get(var Version: byte; var CommandsSupported: TCommands): boolean;
   var i, len: longint;
   begin
      result := false;
      if not SendCommand(CMD_Get) then exit;
      len := ser.RecvByte(Timeout);
      if len < 1 then raise exception.Create('Unsupported value in GET');
      version := ser.RecvByte(Timeout);
      for i := 0 to len-1 do
      begin
         if i > 10 then
            ser.RecvByte(Timeout)
         else
            CommandsSupported[i] := ser.RecvByte(timeout);
      end;
      result := WaitforACK;
   end;

   function GetID(var ID: word): boolean;
   var i, len: longint;
   begin
      result := false;
      if not SendCommand(commands[CO_GetID]) then exit;
      len := ser.RecvByte(timeout);
      id := 0;
      for i := 0 to len do
         id := (id shl 8) or ser.RecvByte(timeout);
      result := WaitforACK;
   end;

   function CalcChecksum(const Data; Size: longint): byte;
   var i: longint;
   begin
      result := 0;
      for i := 0 to size-1 do
         result := result xor pbyte(@data)[i];
   end;

   function AlignToDWord(val: longword): longword;
   begin
      result := (val+3) and $FFFFFFFC;
   end;

   function GlobalErase: boolean;
   begin
      result := false;
      if not SendCommand(commands[CO_Erase]) then exit;
      ser.SendByte($FF);
      ser.SendByte($00);
      result := WaitforACK;
   end;

   function WriteMemory(Address: longword; const Data; Size: longint): boolean;
   var i: longint;
   begin
      result := false;
      if not SendCommand(commands[CO_WriteMem]) then exit;

      //Transmit address+checksum
      for i := 0 to 3 do
         ser.SendByte((address shl (i*8)) shr 24);
      ser.SendByte(CalcChecksum(Address,4));
      if not WaitforACK then raise Exception.Create('Did not get ACK while writing');

      //Transmit size+data+checksum
      ser.SendByte(size-1);
      ser.SendBuffer(@data,size);
      ser.SendByte(CalcChecksum(Data, size) XOR (size-1));

      result := WaitforACK;
   end;

   function ReadMemory(Address: longword; var Data; Size: longint): boolean;
   var i: longint;
   begin
      result := false;
      if not SendCommand(commands[CO_ReadMem]) then exit;

      //Transmit address+checksum
      for i := 0 to 3 do
         ser.SendByte((address shl (i*8)) shr 24);
      ser.SendByte(CalcChecksum(Address,4));
      if not WaitforACK then raise Exception.Create('Did not get ACK while reading');

      //Transmit size+data+checksum
      ser.SendByte(size-1);
      ser.SendByte($FF XOR (size-1));
      if not WaitforACK then raise Exception.Create('Did not get ACK while reading');

      result := ser.RecvBufferEx(@data, size, timeout) = size;
   end;

   procedure CompareMem(Size: longint);
   var i: longint;
   begin
      for i := 0 to size-1 do
         if pbyte(@buffer[0])[i] <> pbyte(@buffer2[0])[i] then
            raise exception.CreateFmt('Error while verifying memory at address $%x. Got $%2x, but expected $%2x', [i+addr, pbyte(@buffer2[0])[i], pbyte(@buffer[0])[i]]);
   end;

begin
   ser := TBlockSerial.Create;
   try
      ser.Connect(ComboBox1.text);
      ser.Config(StrToInt(edit3.text), 8, 'E', SB1, false,false);
      ser.RaiseExcept := true;

      ser.Purge;

      Initialize;

      if Get(version, commands) then
         memo1.lines.add(Format('Version: %d.%d', [version shr 4, version and $F]))
      else
         raise exception.Create('Failed to get version and commands supported');

      if GetID(ID) then
         memo1.lines.add(Format('PID: %x', [id]))
      else
         raise exception.Create('Failed to get product ID');

      if GlobalErase then
         memo1.lines.add('Erased everything')
      else
         raise exception.Create('Failed to do a global erase');

      fs := TFileStream.Create(Edit1.Text, fmOpenRead);
      try
         elf := TElfObject.Create(fs);
         try
            memo1.lines.add('Writing FLASH');
            for i := 0 to elf.SectionCount-1 do
            begin
               memo1.lines.add(' Writing section '+inttostr(i));
               sz := elf.SectionSize[i];
               addr := elf.SectionAddress[i];
               i2 := 0;
               while sz > 0 do
               begin
                  FillDWord(buffer[0], 64, $FFFFFFFF);
                  rd := elf.ReadSection(i, i2, buffer[0], min(sz, 256));
                  if (rd < 256) then
                  begin
                     if rd < sz then
                        rd := rd and $FFFFFFFC
                     else
                        rd := AlignToDWord(rd);
                  end;

                  if not WriteMemory(addr,buffer[0], rd) then raise exception.CreateFmt('Failed to write %d bytes at %x', [rd, addr]);
                  if not ReadMemory(addr,buffer2[0], rd) then raise exception.CreateFmt('Failed to read %d bytes at %x', [rd, addr]);

                  CompareMem(rd);

                  dec(sz,rd);
                  inc(addr,rd);
                  inc(i2,rd);
               end;
            end;
            memo1.lines.add('Done writing');
         finally
            elf.free;
         end;
      finally
         fs.free;
      end;
   finally
      ser.Free;
   end;
end;

procedure TForm1.GotData;
begin
   EnterCriticalsection(form1.fBufferMutex);
   Memo2.Lines.Text := memo2.Lines.Text+fBuffer;
   fBuffer := '';
   LeaveCriticalsection(form1.fBufferMutex);
end;

end.

