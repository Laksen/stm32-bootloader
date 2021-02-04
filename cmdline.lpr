program cmdline;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, math,
  elfobj, synaser
  { you can add units after this };

type

  { TStmBooter }

  TStmBooter = class(TCustomApplication)
  protected
    procedure DoRun; override;
    procedure DoProgram(const Device, Filename: string; Baudrate: longint);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TStmBooter }

procedure TStmBooter.DoRun;
const
  ShortOpts = 'hd:';
  LongOpts: array[0..1] of string = ('help','device:');
var
  ErrorMsg, Device, Fn: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions(ShortOpts, LongOpts);
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') or
     (not HasOption('d', 'device:')) or
     (length(GetNonOptions(ShortOpts, LongOpts))<>1) then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  Device:=GetOptionValue('d','device:');
  Fn:=GetNonOptions(ShortOpts, LongOpts)[0];

  { add your program here }
  DoProgram(Device, Fn, 115200);

  // stop program loop
  Terminate;
end;

procedure TStmBooter.DoProgram(const Device, Filename: string; Baudrate: longint);
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
      ser.Connect(Device);
      ser.Config(BaudRate, 8, 'E', SB1, false,false);
      ser.RaiseExcept := true;

      writeln('Connected');
      ser.Purge;
                           
      writeln('Init');
      Initialize;

      if Get(version, commands) then
         WriteLn(Format('Version: %d.%d', [version shr 4, version and $F]))
      else
         raise exception.Create('Failed to get version and commands supported');

      if GetID(ID) then
         WriteLn(Format('PID: %x', [id]))
      else
         raise exception.Create('Failed to get product ID');

      if GlobalErase then
         WriteLn('Erased everything')
      else
         raise exception.Create('Failed to do a global erase');

      fs := TFileStream.Create(Filename, fmOpenRead);
      try
         elf := TElfObject.Create(fs);
         try
            WriteLn('Writing FLASH');
            for i := 0 to elf.SectionCount-1 do
            begin
               WriteLn(' Writing section '+inttostr(i));
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
            WriteLn('Done writing');
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

constructor TStmBooter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TStmBooter.Destroy;
begin
  inherited Destroy;
end;

procedure TStmBooter.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h -d <device> <filename.elf>');
end;

var
  Application: TStmBooter;
begin
  Application:=TStmBooter.Create(nil);
  Application.Title:='StmBootloader';
  Application.Run;
  Application.Free;
end.

