Public Class Voids
    Public Shared ListOfVoidNames As New ListBox
    Public Shared ListOfCodeOfVoids As New ListBox
    Public Shared ListOfVoidVariables As New ListBox

    Public Shared Function start(line As String, linescounter As Long, code As String)

        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("void ") Then
            'void voidname(argument1 :: argument2){
            Dim voidname As String = FormatConverters.removeSpacesAtBeginningAndEnd(line.Substring(line.ToLower.IndexOf("void") + 4)).Split("(")(0).ToLower
            Dim voidarguments As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line.Substring(line.IndexOf("(")).Substring(0, line.Substring(line.IndexOf("(")).LastIndexOf("{"))))

            If CharacterVerification.Verify(voidname) <> "ok" Then
                'pup error cause invalid characters in voidname : CharacterVerification.Verify(voidname).Split("-")(1)
                Exit Function
            End If

            If Not voidExists(voidname) Then

                ListOfVoidNames.Items.Add(voidname)

                Dim codeOfVoid As String = ""
                Dim actualLine As Long = linescounter + 1
                Dim lineStop As Long = 0
                Dim ouvrantes As Long = 0
                Dim tellToExitFor As Boolean = False

                For i As Long = actualLine To code.Split(ControlChars.Lf).Count - 1

                    Dim getLine As String = FormatConverters.removeSpacesAtBeginningAndEnd(code.Split(ControlChars.Lf)(i))

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
                    Dim getline As String = code.Split(ControlChars.Lf)(countToAddToCodeOfVoid)
                    codeOfVoid += getline & ControlChars.Lf
                Next


                ListOfCodeOfVoids.Items.Add(codeOfVoid)

                '-------------------------------------------------------------------------------------------------'

                If Variables.ClasserExists(voidname.ToLower) Then
                    'pup error cause a classer with the same name as the void voidname is already existing
                Else
                    Variables.CreateClasser(voidname.ToLower)
                End If

                Dim miniCounter As Long = 0

                For Each i As String In voidarguments.Split("::")

                    If Not FormatConverters.isNothingOrSpace(i) Then
                        Dim getVariable As String = FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(i))

                        If getVariable.ToLower.StartsWith("op:") Then

                            If CharacterVerification.Verify(getVariable.Substring(3)) = "ok" Then
                                Variables.CreateVariable(getVariable.Substring(3), "", voidname.ToLower)
                            Else
                                'pup error cause invalid characters in argument (miniCounter + 1) : CharacterVerification.Verify(getVariable.Substring(3)).Split("-")(1)
                                Exit Function
                            End If

                        Else

                            If CharacterVerification.Verify(getVariable) = "ok" Then
                                Variables.CreateVariable(getVariable, "", voidname.ToLower)
                            Else
                                'pup error cause invalid characters in argument (miniCounter + 1) : CharacterVerification.Verify(getVariable).Split("-")(1)
                                Exit Function
                            End If

                        End If


                    End If

                    miniCounter += 1
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
            Dim ArgumentsOfVoid As String = ListOfVoidVariables.Items(indexOfVoidName).ToString.Replace("::", "{separator_of_arguments}")
            Dim CodeOfVoid As String = ListOfCodeOfVoids.Items(indexOfVoidName)
            Dim counter As Long = 0



            Dim countOfNecessaryArgs As Long = 0

            For Each countingNecessaryArgs As String In ArgumentsOfVoid.ToString.Split("{separator_of_arguments}")
                Dim iss As String = countingNecessaryArgs

                If iss.StartsWith("separator_of_arguments}") Then
                    iss = iss.Substring(23)
                End If

                If Not FormatConverters.removeSpacesAtBeginningAndEnd(iss).ToLower.StartsWith("op:") _
                    AndAlso Not FormatConverters.isNothingOrSpace(FormatConverters.removeSpacesAtBeginningAndEnd(iss)) Then

                    countOfNecessaryArgs += 1

                End If

            Next

            Dim listOfArgs As New List(Of String)
            Dim countOfArgsByUser As Long = 0

            For Each i As String In arguments.Split("::")
                If Not FormatConverters.isNothingOrSpace(i) Then
                    listOfArgs.Add(i)
                    countOfArgsByUser += 1
                End If
            Next


            If countOfArgsByUser < countOfNecessaryArgs Then
                'pup error cause arguments are missing for the void
                Exit Sub
            End If


            For Each issOriginal As String In ArgumentsOfVoid.ToString.Split("{separator_of_arguments}")

                Dim iss As String = issOriginal

                If iss.StartsWith("separator_of_arguments}") Then
                    iss = iss.Substring(23)
                End If

                Dim i As String = FormatConverters.removeSpacesAtBeginningAndEnd(iss)

                If Not FormatConverters.isNothingOrSpace(i) Then

                    If Not i.StartsWith("op:") Then
                        Try

                            Dim b As String = listOfArgs(counter)

                        Catch ex As Exception
                            'pup error cause the value of the argument i for the void voidname is missing
                            Exit Sub
                        End Try

                        Dim getExpressionOfArgument As String = FormatConverters.getExpression(listOfArgs(counter))
                        Variables.EditVariable(i, getExpressionOfArgument, voidname.ToLower)
                    Else
                        Try
                            Dim getExpressionOfArgument As String = FormatConverters.getExpression(listOfArgs(counter))
                            Variables.EditVariable(i.Substring(3), getExpressionOfArgument, voidname.ToLower)
                        Catch

                        End Try
                    End If


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