Imports System.IO
Public Class FormatConverters
    Public Shared Function removespacesAtTheEnd(inputs As String) As String
        Try

            Dim counter As Integer = 0
            Dim input = StrReverse(inputs)
            For Iss = 0 To input.Length - 1
                If input(Iss) = " " Then
                    counter += 1
                Else
                    Exit For
                End If
            Next
            '    MsgBox("counter: " & counter)
            '   MsgBox("output: " & input.Substring(0, input.Length - counter).Replace(" ", "{SPACE}"))
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
    Public Shared Function ConvertToAbleToRead(input As String) As String
        Dim realInput = input
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
            MsgBox("inputerror: " & input)
        End Try
        Return realInput
    End Function
    Public Shared Function RemoveQuotes(input As String) As String
        Dim realInput = input
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
    Public Shared Function getClasserAndVariableDelimited(input As String, classerOrVariable As String)
        If input.Contains(";;") Then

            Dim counter = 0
            Dim nexts = False
            Dim finalexpressionVariable = ""
            Try
                Dim varName = ""
                Dim varClasser = ""
                For Each i2 In input
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
                '   MsgBox("ex" & ex.Message)
            End Try
        End If
        Return ""
    End Function
    Public Shared Function splitObjectsCorrectlyInALine(input As String) As List(Of String)
        Dim listofstringformyfunction As New List(Of String)
        Dim tagSkipCounter = 0
        Dim finalobject = ""
        '  MsgBox("input: " & input)
        For i22 = 0 To input.Split(",").Count - 1
            Dim i = input.Split(",")(i22)
            Dim objetfictif = ""
            '  MsgBox("i: " & tagSkipCounter.ToString)
            If tagSkipCounter > 0 Then
                tagSkipCounter -= 1
            Else
                '  MsgBox("i: " & i)
                For Each i2 As String In removeSpacesAtBeginningAndEnd(i)
                    If i2 = "#" Then
                        tagSkipCounter += 1
                    Else
                        If tagSkipCounter > 0 Then
                            objetfictif += i '& ","
                        Else
                            ' MsgBox("wtftagskipcounter: " & tagSkipCounter & " /" & i)
                            objetfictif += i ' virgule avant
                        End If
                        Exit For
                    End If

                Next
                Try
                    For michaelJackson = 1 To tagSkipCounter
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
        Dim counterOfHashTags = 0
        Dim input = removeSpacesAtBeginningAndEnd(inputs)
        For i55 = 0 To input.Length - 1
            Dim i = input(i55)
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
    Public Shared Function getExpression(input As String) As String
        Dim realInput = ConvertToAbleToRead(removeSpacesAtBeginningAndEnd(input))
        Dim finalOutput = ""
        Dim BigFinalOutput = ""
        Dim listOfStringForMyFunction = splitObjectsCorrectlyInALine(input)
        Try
            For Each i As String In listOfStringForMyFunction
                Dim realI = ConvertToAbleToRead(removeSpacesAtBeginningAndEnd(i))
                If realI.StartsWith("""") Then
                    Dim realIConvert = RemoveQuotes(realI)
                    finalOutput += realIConvert
                ElseIf realI.StartsWith("&") Then
                    Dim getFunctionCallName = FormatConverters.removeSpacesAtBeginningAndEnd(realI.Substring(1).Substring(0, realI.IndexOf("(") - 1)).ToLower
                    If Not Functions.listOfFunctionsNames.Items.Contains(getFunctionCallName) Then
                        finalOutput += IntegratedFunctions.getFunctionCall(realI)
                    Else
                        Dim getIndexToSearch = Functions.listOfFunctionsNames.Items.IndexOf(getFunctionCallName.ToLower)
                        Dim code = Functions.listOfFunctionsCodes.Items(getIndexToSearch)
                        Dim getArguments = Functions.listOfFunctionsArguments.Items(getIndexToSearch)
                        Dim realIWithoutArraySpecified = realI.Substring(1)
                        Dim realISpecifiedArray = ""
                        If realI.EndsWith("]") Then
                            If realI.Contains("[") Then
                                realISpecifiedArray = realI.Substring(realI.LastIndexOf("[")).Replace("[", "").Replace("]", "")
                                realIWithoutArraySpecified = realI.Substring(1, realI.LastIndexOf("[") - 1)
                            Else
                                'pup error cause syntax error, remove ] at end of object
                            End If
                        End If



                        'Dim realIAbleToRead = FormatConverters.ConvertToAbleToRead(realI)

                        Dim getArgumentsByUser = FormatConverters.ConvertToAbleToRead(realIWithoutArraySpecified.Substring(realIWithoutArraySpecified.IndexOf("(")))
                        Dim getArgumentsOfFunction = Functions.listOfFunctionsArguments.Items(getIndexToSearch)

                        Dim listOfSplitArgsFunction As New List(Of String)

                        For Each i5 In getArgumentsOfFunction.ToString.Split("::")
                            If Not isNothingOrSpace(i5) Then
                                listOfSplitArgsFunction.Add(i5)
                            End If
                        Next

                        Dim countOfArgsByUser = 0
                        For Each i4 As String In getArgumentsByUser.Split("::")
                            If Not isNothingOrSpace(i4) Then
                                Try
                                    If countOfArgsByUser = listOfSplitArgsFunction.Count Then
                                        'pup error cause too many arguments for the function
                                        Exit Function
                                    End If
                                    Dim getExpressionOfI4 = getExpression(ConvertToAbleToRead(i4))
                                        Dim getDistantVariable = FormatConverters.removeSpacesAtBeginningAndEnd(listOfSplitArgsFunction(countOfArgsByUser))


                                        'MsgBox("variabletoedit: " & getDistantVariable)
                                        ' MsgBox("newvalue: " & getExpressionOfI4)
                                        'MsgBox("getfunctioncallname: " & getFunctionCallName)
                                        Variables.EditVariable(getDistantVariable, getExpressionOfI4, getFunctionCallName)
                                        countOfArgsByUser += 1

                                Catch ex As IndexOutOfRangeException
                                    'pup error cause arguments are missing for function
                                    Exit Function
                                End Try
                            End If
                        Next



                        ' MsgBox("getargumentsbyuser: " & getArgumentsByUser)
                        ' MsgBox("getargumentsoffunction: " & getArgumentsOfFunction)

                        If Functions.checkIfReturnsSomething(code) Then
                            Interpret.Start(code)
                        Else
                            'pup error cause function returns nothing
                        End If

                        Dim getReturnsList = Functions.listOfFunctionReturns.Items(Functions.listOfFunctionsNames.Items.IndexOf(getFunctionCallName.ToLower))
                        If getReturnsList.ToString.EndsWith("µ") Then
                            getReturnsList = getReturnsList.ToString.Substring(0, getReturnsList.ToString.Length - 1)
                        End If

                        If IsNumeric(realISpecifiedArray) Then
                            Try
                                finalOutput += getReturnsList.ToString.Split("µ")(Long.Parse(realISpecifiedArray))
                            Catch ex As IndexOutOfRangeException
                                'pup error cause specified return array isn't existing
                                Exit Function
                            End Try
                        Else
                            finalOutput += getReturnsList.ToString.Split("µ")(0)
                        End If
                    End If
                        ElseIf realI.StartsWith("$") Then
                    'define(vc) b = "hello";
                    'print($salut;;dc)
                    'print($salutations;;dc, "Est le spécimen de ", ##&Math.ComputeExpression("5 + ", "5 + ", "8"))
                    Dim variableName = getClasserAndVariableDelimited(realI.Substring(1), "variable")
                    Dim classer = getClasserAndVariableDelimited(realI.Substring(1), "classer").ToString.ToLower
                    finalOutput += Variables.GetVariable(variableName, classer)
                Else
                    finalOutput += realI
                End If
            Next
        Catch ex As Exception
            MsgBox(ex.StackTrace)
            MsgBox(ex.Message)
        End Try

        Return finalOutput
    End Function
End Class