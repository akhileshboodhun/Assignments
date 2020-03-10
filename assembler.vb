Imports System
Imports System.IO
Module Assembler
Dim FileName as String
Dim instruction(1000) As String
Dim opcode(1000) As String
Dim operand(1000) As String
Dim ACC As String = ""
Dim IX As String = ""
Dim Address As Integer
Dim temps as Integer
Dim memloc as Integer
Dim CmpFlag as Boolean = False
Dim OutputSCR As String = ""
Dim EOFPos as integer
Dim counter as integer
dim keyinput as String = ""
Dim filechanges as Boolean
Dim tempfile as string
Dim newfile as string
Sub Main()
Console.WindowHeight = Console.LargestWindowHeight - 15
Console.WindowWidth = Console.LargestWindowWidth - 15
Console.ForegroundColor = ConsoleColor.Cyan
Console.WriteLine("Visual Basic.Net Compiler version 0.0.0.5943 (Mono 4.6 - tarball)")
Console.Writeline("Copyright (C) 2004-2010 Rolf Bjarne Kvinge. All rights reserved.")
Console.Writeline("Simple Assembler Built By Akhilesh Boodhun.")
Console.Writeline("STATUS: Not Completed!")
Console.Write("Usage of text file: ")
Console.ForegroundColor = ConsoleColor.Green
Console.Write("<Address>")
Console.ForegroundColor = ConsoleColor.Magenta
Console.Write("<Space>")
Console.ForegroundColor = ConsoleColor.Green
Console.Write("<Opcode>")
Console.ForegroundColor = ConsoleColor.Magenta
Console.Write("<Space>")
Console.ForegroundColor = ConsoleColor.Green
Console.Writeline("<Operand>")
Console.ForegroundColor = ConsoleColor.Cyan
Console.Write("Example: ")
Console.ForegroundColor = ConsoleColor.Green
Console.Writeline("093 LDM #178")
Console.ForegroundColor = ConsoleColor.Cyan
Console.Write("Press ")
Console.ForegroundColor = ConsoleColor.White
Console.Write("[ENTER]")
Console.ForegroundColor = ConsoleColor.Cyan
Console.Writeline(" to continue...")
Console.Readkey()
Console.Clear()
LineInput: Console.ForegroundColor = ConsoleColor.Cyan
 Console.Write("Enter Filename: ")
Console.ForegroundColor = ConsoleColor.Yellow
FileName = Console.ReadLine()
If Not(My.Computer.FileSystem.FileExists(FileName)) Then
Console.ForegroundColor = ConsoleColor.Red
Console.Writeline("Error: File doesn't exist!")
GoTo LineInput
End If
ReadAsm()
Console.Write("Display file? (Y/N):")
keyinput = console.readline()
If (keyinput = "Y" ) OR ( keyinput = "y") Then
DisplayFile()
End If
keyinput=""
ExecAsm()
Address = 0
Console.Write("Accumulator: ")
Console.ForegroundColor = ConsoleColor.Magenta
Console.Writeline(ACC)
Console.ForegroundColor = ConsoleColor.Yellow
Console.Write("Index Register: ")
Console.ForegroundColor = ConsoleColor.Magenta
Console.Writeline(IX)
Console.ForegroundColor = ConsoleColor.Yellow
Console.Write("Output: ")
Console.ForegroundColor = ConsoleColor.Magenta
Console.Writeline(OutputSCR)
Console.ForegroundColor = ConsoleColor.Green
StoreTemp()
filechanges = False
filechanges = CompareFiles(FileName , tempfile)
Console.ForegroundColor = ConsoleColor.Cyan
'If filechanges = True Then
Console.Write("Store changes in a new file?(Y/N):")
Console.ForegroundColor = ConsoleColor.Yellow
keyinput = console.readline()
If (keyinput = "Y" ) OR ( keyinput = "y") Then
StoreNewFile()
End If
'End If
keyinput=""
Console.ReadKey()
End Sub

Sub ReadAsm()
EOFPos = 0
Address = 0
FileOpen(1, Filename, OpenMode.Input)
Do
Input(1, instruction(Address))
opcode(Address) = Mid(instruction(Address),5,3)
operand(Address) = Mid(instruction(Address),9,(Len(instruction(Address))))
Address = Address + 1
EOFPos = EOFPos + 1
Loop Until (EOF(1)=True)
FileClose(1)
End Sub

Sub ExecAsm()
Address = 0
Do
CallInstruction(Address)
Address = Address + 1
Loop Until (Address=100) OR (opcode(Address)="END")
End Sub
Sub CallInstruction(ByVal Address2 As Integer)
Select Case opcode(Address2)
 Case "LDM" : LDM1(operand(Address2))
 Case "LDD" : LDD1(operand(Address2))
 Case "LDI" : LDI1(operand(Address2))
 Case "LDX" : LDX1(operand(Address2))
 Case "LDR" : LDR1(operand(Address2))
 Case "STO" : STO1(operand(Address2))
 Case "INC" : INC1(operand(Address2))
 Case "DEC" : INC1(operand(Address2))
 Case "CMP" : CMP1(operand(Address2))
 Case "JPE" : JPE1(operand(Address2))
 Case "JPN" : JPN1(operand(Address2))
 Case "AND" : AND1(operand(Address2))
 Case "XOR" : XOR1(operand(Address2))
 Case "OR" : OR1(operand(Address2))
 Case "IN" : IN1(operand(Address2))
 Case "OUT" : OUT1(operand(Address2))
 Case "ADD" : ADD1(operand(Address2))
 Case "SUB" : SUB1(operand(Address2))
 Case "MUL" : MUL1(operand(Address2))
 Case "DIV" : DIV1(operand(Address2))
 Case Else: 'Do nothing
End Select
End Sub

Sub LDM1(ByVal x As String)
If (Left(x,1) ="#") Then
ACC = Mid(x,2,(Len(x)-1))
End If
End Sub

Sub LDD1(ByVal x As String)
temps = CInt(x)
ACC = (Mid( instruction(temps) ,5,( Len( instruction(temps) ) )))
End Sub

Sub LDX1(ByVal x As String)
temps = CInt(x) + CInt(IX)
ACC = (Mid( instruction(temps) ,5,( Len( instruction(temps) ) )))
End Sub

Sub LDR1(ByVal x As String)
If (Left(x,1) ="#") Then
IX = Mid(x,2,(Len(x)-1))
End If
End Sub
Sub LDI1(ByVal x As String)
memloc = CInt(x)
temps = CInt(Mid(instruction(memloc),5,Len(instruction(memloc))))
LDD1(Convert.ToString(temps))
End Sub

Sub STO1(ByVal x As String)
temps= CInt(x)
instruction(temps) = Left(instruction(temps),5) & ACC
End Sub

Sub INC1(ByVal x As String)
If x = "ACC" Then
ACC = Convert.ToString ((CInt(ACC) + 1))
Else
IX = Convert.ToString ((CInt(IX) + 1))
End If
End Sub

Sub DEC1(ByVal x As String)
If x = "ACC" Then
ACC = Convert.ToString ((CInt(ACC) - 1))
Else
IX = Convert.ToString ((CInt(IX) - 1))
End If
End Sub

Sub CMP1(ByVal x As String)
If (Left(x,1) ="#") Then
    temps = (CInt(Mid(x,2,Len(x))))
Else
memloc = CInt(Left(x,3))
temps =(CInt( Mid(instruction(memloc),4, ( Len(instruction(memloc) ) ) ) ))
End If
If (temps) =(CInt(ACC)) Then
        CmpFlag = True
        Else
        CmpFlag = False
    End If
End Sub

Sub JPE1(ByVal x As String)
If (CmpFlag = True) Then
Address = CInt(x)
End If
End Sub

Sub JPN1(ByVal x As String)
If (CmpFlag = False) Then
Address = CInt(x)
End If
End Sub

Sub AND1(ByVal x As String)
MUL1(x)
End Sub

Sub XOR1(ByVal x As String)
temps = CInt(x)
End Sub

Sub OR1(ByVal x As String)
ADD1(x)
End Sub

Sub IN1(ByVal x As String)
Console.Write("Input Char:")
Dim randominput as string
randominput = Console.Readline()
If randominput <> "" then
ACC = Convert.ToString(ASC(randominput))
Else
Console.ForegroundColor = ConsoleColor.Red
Console.Writeline("Error: No input found!")
Console.ForegroundColor = ConsoleColor.Yellow
End If
End Sub

Sub OUT1(ByVal x As String)
temps = CInt(ACC)
OutputSCR = Chr(temps)
End Sub

Sub ADD1(ByVal x As String)
If (Left(x,1) ="#") Then
temps = CInt(ACC) + (CInt(Mid(x,2,Len(x))))
ACC = Convert.ToString(temps)
Else
memloc = CInt(Left(x,3))
temps = CInt(ACC) + (CInt( Mid(instruction(memloc),4, ( Len(instruction(memloc) ) ) ) ))
ACC = Convert.ToString(temps)
End If
End Sub

Sub SUB1(ByVal x As String)
If (Left(x,1) ="#") Then
temps = CInt(ACC) - (CInt(Mid(x,2,Len(x))))
ACC = Convert.ToString(temps)
Else
memloc = CInt(Left(x,3))
temps = CInt(ACC) - (CInt( Mid(instruction(memloc),4, ( Len(instruction(memloc) ) ) ) ))
ACC = Convert.ToString(temps)
End If
End Sub

Sub MUL1(ByVal x As String)
If (Left(x,1) ="#") Then
temps = CInt(ACC) * (CInt(Mid(x,2,Len(x))))
ACC = Convert.ToString(temps)
Else
memloc = CInt(Left(x,3))
temps = CInt(ACC) * (CInt( Mid(instruction(memloc),4, ( Len(instruction(memloc) ) ) ) ))
ACC = Convert.ToString(temps)
End If
End Sub
Sub JMP(ByVal x As String)
Address = CInt(x)
End Sub

Sub DIV1(ByVal x As String)
If (Left(x,1) ="#") Then
temps = CInt(ACC) / (CInt(Mid(x,2,Len(x))))
ACC = Convert.ToString(temps)
Else
memloc = CInt(Left(x,3))
temps = CInt(ACC) / (CInt( Mid(instruction(memloc),4, ( Len(instruction(memloc) ) ) ) ))
ACC = Convert.ToString(temps)
End If
End Sub
Sub DisplayFile()
For counter = 0 to EOFPos
Console.ForegroundColor = ConsoleColor.Green
Console.Writeline(instruction(counter))
Next
Console.ForegroundColor = ConsoleColor.Yellow
End Sub
Sub StoreNewFile()
newfile = "new" & FileName
Console.ForegroundColor = ConsoleColor.Magenta
If (My.Computer.FileSystem.FileExists(newfile)) Then
Console.Writeline("The new file has been overwritten!")
Else
Console.Writeline("The new file has been created!")
Console.ForegroundColor = ConsoleColor.Yellow
End If
FileOpen(2,newfile,OpenMode.Output)
For counter = 0 to EOFPos
Printline(2, instruction(counter))
Next
FileClose(2)
End Sub
Sub StoreTemp()
tempfile = ".new" & FileName & ".temp"
FileOpen(3,tempfile,OpenMode.Output)
For counter = 0 to EOFPos
Printline(3, instruction(counter))
Next
FileClose(3)
End Sub
Public Function CompareFiles(ByVal file1FullPath As String, ByVal file2FullPath As String) As Boolean

If Not File.Exists(file1FullPath) Or Not File.Exists(file2FullPath) Then
    'One or both of the files does not exist.
    Return False
End If

If file1FullPath = file2FullPath Then
    ' fileFullPath1 and fileFullPath2 points to the same file...
    Return True
End If

Try
    Dim file1Hash as String = hashFile(file1FullPath)
    Dim file2Hash as String = hashFile(file2FullPath)

    If file1Hash = file2Hash Then
        Return True
    Else
        Return False
    End If

Catch ex As Exception
    Return False
End Try
End Function

Private Function hashFile(ByVal filepath As String) As String
    Using reader As New System.IO.FileStream(filepath, IO.FileMode.Open, IO.FileAccess.Read)
        Using md5 As New System.Security.Cryptography.MD5CryptoServiceProvider
            Dim hash() As Byte = md5.ComputeHash(reader) 
            Return System.Text.Encoding.Unicode.GetString(hash) 
        End Using
    End Using
End Function
End Module
