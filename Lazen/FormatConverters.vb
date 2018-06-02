Imports System.IO
Public Class FormatConverters
    Public Shared Function removespacesAtTheEnd(inputs As String) As String
        Try

            Dim counter As Integer = 0
            Dim input As String = StrReverse(inputs)
            For Iss As Long = 0 To input.Length - 1
                If input(Iss) = " " Then
                    counter += 1
                Else
                    Exit For
                End If
            Next

            Return inputs.Substring(0, inputs.Length - counter)
        Catch ex As Exception
            'fatal error while removing spaces at end, try to remove spaces at the end of the argument
            Return ""
        End Try

    End Function
    Public Shared Function removeSpacesAtBeginningAndEnd(input As String) As String
        Dim counter As Integer = 0

        For Each i As String In input
            If i = " " Then
                counter += 1
            Else
                Exit For
            End If
        Next
        Return removespacesAtTheEnd(input.Substring(counter))
    End Function
    Public Shared Function getBeforeParenthesis(input As String) As String
        Dim result As String = input

        If input.ToLower.Contains("(") Then
            result = result.Substring(0, result.IndexOf("("))
        End If

        Return result
    End Function
    Public Shared Function ConvertToAbleToRead(input As String) As String
        Dim realInput As String = input
        Try
            If realInput.EndsWith(";") Then
                realInput = realInput.Substring(0, realInput.Length - 1)
            End If
            If realInput.StartsWith("(") Then
                realInput = realInput.Substring(1)
            End If
            If realInput.EndsWith(")") Then
                realInput = realInput.Substring(0, realInput.Length - 1)
            End If
        Catch

        End Try
        Return realInput
    End Function
    Public Shared Function RemoveQuotes(input As String) As String
        Dim realInput As String = input
        If realInput.StartsWith("""") Then
            realInput = realInput.Substring(1)
        End If
        If realInput.EndsWith("""") Then
            realInput = realInput.Substring(0, realInput.Length - 1)
        End If
        Return realInput
    End Function
    Public Shared Function isNothingOrSpace(input As String)
        For Each i As String In input
            If i <> "" AndAlso i <> " " Then

                Return False
                Exit Function

            End If
        Next
        Return True
    End Function
    Public Shared Function getClasserAndVariableDelimited(input As String, classerOrVariable As String) As String
        If input.Contains(";;") Then

            Dim counter As Long = 0
            Dim nexts = False
            Dim finalexpressionVariable As String = ""
            Try
                Dim varName As String = ""
                Dim varClasser As String = ""
                For Each i2 As String In input
                    If i2 = ";" Then
                        If input(counter + 1) = ";" Then
                            'msgBox("taki")
                            varName = ConvertToAbleToRead(removeSpacesAtBeginningAndEnd(input.Substring(0, counter + 1)))
                            varClasser = ConvertToAbleToRead(removeSpacesAtBeginningAndEnd(input.Substring(counter + 2)))
                            ' MsgBox(varName & "/" & varClasser)
                        End If
                    End If
                    counter += 1
                Next

                If classerOrVariable.ToLower = "classer" Then
                    Return varClasser
                    Exit Function
                ElseIf classerOrVariable.ToLower = "variable" Then
                    Return varName
                    Exit Function
                End If

            Catch ex As Exception

            End Try
        End If
        Return ""
    End Function
    Public Shared Function splitObjectsCorrectlyInALine(input As String) As List(Of String)
        Dim listofstringformyfunction As New List(Of String)
        Dim tagSkipCounter As Long = 0
        Dim finalobject As String = ""

        For i22 = 0 To input.Split(",").Count - 1
            Dim i As String = input.Split(",")(i22)
            Dim objetfictif As String = ""

            If tagSkipCounter > 0 Then
                tagSkipCounter -= 1
            Else

                For Each i2 As String In removeSpacesAtBeginningAndEnd(i)
                    If i2 = "#" Then
                        tagSkipCounter += 1
                    Else
                        If tagSkipCounter > 0 Then
                            objetfictif += i '& ","
                        Else

                            objetfictif += i ' virgule avant
                        End If
                        Exit For
                    End If

                Next
                Try
                    For michaelJackson As Long = 1 To tagSkipCounter
                        objetfictif += "," & input.Split(",")(i22 + michaelJackson)
                    Next
                Catch
                    'pup error cause too many # before object
                End Try
            End If
            listofstringformyfunction.Add(removeHashTagsAtTheBeginning(objetfictif))
        Next
        Return listofstringformyfunction
    End Function
    Public Shared Function removeHashTagsAtTheBeginning(inputs As String) As String
        Dim counterOfHashTags As Long = 0
        Dim input As String = removeSpacesAtBeginningAndEnd(inputs)
        For i55 As Long = 0 To input.Length - 1
            Dim i As String = input(i55)
            If i = "#" Then
                counterOfHashTags += 1
            Else
                Exit For
            End If
        Next
        Return input.Substring(counterOfHashTags)
    End Function
    Public Shared Function convertTrueFalse(inputs As Boolean) As String
        If inputs.ToString.ToLower = "true" Then
            Return "1"
        ElseIf inputs.ToString.ToLower = "false" Then
            Return "0"
        Else
            Return inputs.ToString
        End If
    End Function
    Public Shared Function getExpression(inputRaw As String) As String

        Dim input As String = inputRaw

        If input.EndsWith(";") Then 'check if input ends with a semicolon
            input = input.Substring(0, input.Length - 1) 'remove the semicolon
        End If

        Dim realInput As String = ConvertToAbleToRead(removeSpacesAtBeginningAndEnd(input))
        Dim finalOutput As String = ""
        Dim BigFinalOutput As String = ""
        Dim listOfStringForMyFunction As List(Of String) = splitObjectsCorrectlyInALine(input)

        Try

            For Each i As String In listOfStringForMyFunction

                Dim realI As String = ConvertToAbleToRead(removeSpacesAtBeginningAndEnd(i))

                If realI.StartsWith("""") Then

                    Dim realIConvert As String = RemoveQuotes(realI)
                    finalOutput += realIConvert

                ElseIf realI.StartsWith("&") Then

                    Dim getFunctionCallName As String = FormatConverters.removeSpacesAtBeginningAndEnd(realI.Substring(1).Substring(0, realI.IndexOf("(") - 1)).ToLower

                    If Not Functions.listOfFunctionsNames.Items.Contains(getFunctionCallName) Then

                        finalOutput += IntegratedFunctions.getFunctionCall(realI)

                    Else
                        'print(&myFunction(argument :: argument));

                        Dim normalI As String = removeSpacesAtBeginningAndEnd(i).Substring(1)

                        If Not normalI.Contains("(") AndAlso normalI.Contains(")") Then
                            'pup error cause function call is wrongly built
                            Exit Function
                        End If

                        Dim FunctionIndex As Long = Functions.listOfFunctionsNames.Items.IndexOf(getFunctionCallName.ToLower).ToString
                        Dim FunctionCode As String = Functions.listOfFunctionsCodes.Items(FunctionIndex).ToString.Replace(vbTab, "")
                        Dim lineStartForCopy As String = Functions.lineStartForCopy.Items(FunctionIndex).ToString
                        Dim FunctionArguments As String = Functions.listOfFunctionsArguments.Items(FunctionIndex).ToString.ToLower
                        Dim FunctionArgumentsList As New List(Of String)

                        'functionName = functionCallName
                        Dim UserArguments As String
                        If normalI.Replace(" ", "").EndsWith(")") Then
                            UserArguments = normalI.Substring(normalI.IndexOf("(") + 1, normalI.Substring(normalI.IndexOf("(")).LastIndexOf(")") - 1)
                        Else
                            UserArguments = normalI.Substring(normalI.IndexOf("(") + 1)
                        End If
                        Dim UserArgumentsList As New List(Of String)

                        For Each i2 As String In UserArguments.Split("::")
                            If Not isNothingOrSpace(i2) Then
                                UserArgumentsList.Add(removeSpacesAtBeginningAndEnd(i2))
                            End If
                        Next

                        For Each i3 As String In FunctionArguments.Split("::")
                            If Not isNothingOrSpace(i3) Then
                                FunctionArgumentsList.Add(removeSpacesAtBeginningAndEnd(i3))
                            End If
                        Next

                        If Not FunctionArgumentsList.Count = UserArgumentsList.Count Then
                            'pup error cause arguments are missing
                            Exit Function
                        End If

                        Dim counter As Integer = 0

                        For Each i4 As String In FunctionArgumentsList
                            Variables.EditVariable(i4.ToLower, getExpression(UserArgumentsList(counter)), getFunctionCallName.ToLower)
                            counter += 1
                        Next

                        Interpret.Start(FunctionCode, True, removeSpacesAtBeginningAndEnd(getFunctionCallName.ToLower))

                        Dim returnResult As String = Functions.listOfFunctionReturns.Items(FunctionIndex)

                        finalOutput += returnResult

                    End If
                ElseIf realI.StartsWith("$") Then

                    'define(vc) b = "hello";
                    'print($salut;;dc)
                    'print($salutations;;dc, " example ", ##&Math.ComputeExpression("5 + ", "5 + ", "8"))

                    Dim variableName As String = getClasserAndVariableDelimited(realI.Substring(1), "variable")
                    Dim classer As String = getClasserAndVariableDelimited(realI.Substring(1), "classer").ToString.ToLower
                    finalOutput += Variables.GetVariable(variableName, classer)
                Else
                    finalOutput += realI
                End If
            Next

        Catch ex As Exception
            Return "ERROR-IN-GETEXPRESSION"
        End Try

        Return finalOutput
    End Function
End Class