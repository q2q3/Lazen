
Public Class Voids
    Public Shared ListOfVoidNames As New ListBox
    Public Shared ListOfCodeOfVoids As New ListBox
    Public Shared ListOfVoidVariables As New ListBox

    Public Shared Function start(line As String, linescounter As Long, code As String)

        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("void ") Then
            'void voidname(argument1 :: argument2){
            Dim voidname As String = FormatConverters.removeSpacesAtBeginningAndEnd(line.Substring(line.ToLower.IndexOf("void") + 4)).Split("(")(0).ToLower
            Dim voidarguments As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line.Substring(line.IndexOf("(")).Substring(0, line.Substring(line.IndexOf("(")).LastIndexOf("{"))))

            If Not voidExists(voidname) Then

                ListOfVoidNames.Items.Add(voidname)

                Dim codeOfVoid As String = ""
                Dim actualLine As Long = linescounter + 1
                Dim lineStop As Long = 0
                Dim ouvrantes As Long = 0
                Dim tellToExitFor As Boolean = False

                For i As Long = actualLine To code.Split(ControlChars.Lf).Count - 1

                    Dim getLine As String = code.Split(ControlChars.Lf)(i)

                    For Each i2 As String In getLine

                        If i2 = "{" Then
                            ouvrantes += 1
                        ElseIf i2 = "}" Then

                            If ouvrantes > 0 Then
                                ouvrantes -= 1
                            Else
                                lineStop = i
                                tellToExitFor = True
                                Exit For
                            End If

                        End If
                    Next

                    If tellToExitFor Then
                        Exit For
                    End If
                Next

                For countToAddToCodeOfVoid = actualLine To lineStop - 1
                    Dim getline = code.Split(ControlChars.Lf)(countToAddToCodeOfVoid)
                    codeOfVoid += getline & ControlChars.Lf
                Next

                ListOfCodeOfVoids.Items.Add(codeOfVoid)

                '-------------------------------------------------------------------------------------------------'

                If Variables.ClasserExists(voidname.ToLower) Then
                    'pup error cause a classer with the same name as the void voidname is already existing
                Else
                    Variables.CreateClasser(voidname.ToLower)
                End If

                For Each i As String In voidarguments.Split("::")

                    If Not FormatConverters.isNothingOrSpace(i) Then
                        Dim getVariable As String = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(i)))
                        Variables.CreateVariable(getVariable, "", voidname.ToLower)
                    End If

                Next

                ListOfVoidVariables.Items.Add(voidarguments)
                Return lineStop.ToString
            Else
                'pup error cause void voidname is already existing
                Exit Function
            End If
        End If
    End Function
    Public Shared Sub startVoid(line As String, linescounter As Long)
        Dim voidSearchExists As Boolean = False

        Try
            voidSearchExists = ListOfVoidNames.Items.Contains(FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("(")))
        Catch ex As Exception
            voidSearchExists = False
        End Try

        If voidSearchExists Then

            Dim voidname As String = FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.Substring(0, FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("("))
            Dim arguments As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(FormatConverters.removeSpacesAtBeginningAndEnd(line).IndexOf("("))))
            Dim indexOfVoidName As Long = ListOfVoidNames.Items.IndexOf(voidname)
            Dim ArgumentsOfVoid As String = ListOfVoidVariables.Items(indexOfVoidName)
            Dim CodeOfVoid As String = ListOfCodeOfVoids.Items(indexOfVoidName)
            Dim counter As Long = 0

            For Each iss As String In ArgumentsOfVoid.ToString.Split("::")

                Dim i = FormatConverters.removeSpacesAtBeginningAndEnd(iss)

                If Not FormatConverters.isNothingOrSpace(i) Then

                    Try

                        Dim b = arguments.Split("::")(counter)

                    Catch ex As Exception
                        'pup error cause the value of the argument i for the void voidname is missing
                        Exit Sub
                    End Try

                    Dim getExpressionOfArgument = FormatConverters.getExpression(arguments.Split("::")(counter))
                    Variables.EditVariable(i, getExpressionOfArgument, voidname.ToLower)

                End If

                counter += 1

            Next
            Interpret.Start(CodeOfVoid)
        End If
    End Sub
    Public Shared Function voidExists(voidname As String) As Boolean
        If ListOfVoidNames.Items.Contains(voidname) Then
            Return True
        Else
            Return False
        End If
    End Function
End Class