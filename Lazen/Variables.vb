Public Class Variables
    Public Shared classers As New ListBox
    Public Shared classersVariablesValues As New ListBox
    'on a classer value: classernameµvarµvarµvar
    'on a classer variablesvalues: varvalueµvarvalueµvarvalue
    Public Shared Sub CreateVariable(variable As String, variableValue As String, classer As String)
        If ClasserExists(classer.ToLower) Then
            If Not VariableExists(variable.ToLower, classer.ToLower) Then
                Dim getindexofclasser = getIndexToClasser(classer.ToLower)
                ' MsgBox("getindexofclasser: " & getindexofclasser)
                Dim SaveClasserContent = classers.Items(getindexofclasser)
                Dim SaveVariablesValuesContent = classersVariablesValues.Items(getindexofclasser)
                Dim getIndexToRemove = getindexofclasser

                classers.Items.RemoveAt(getIndexToRemove)
                classersVariablesValues.Items.RemoveAt(getIndexToRemove)
                'MsgBox("saveclassercontent : " & SaveClasserContent)
                ' MsgBox("savevarvaluescontent : " & SaveVariablesValuesContent)
                classers.Items.Add(SaveClasserContent & variable.ToLower & "µ")
                classersVariablesValues.Items.Add(SaveVariablesValuesContent & "µ" & FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(variableValue))))
            Else
                'pup error cause a variable called 'variable' already exists
            End If
        Else
            'pup error cause classer doesn't exists
        End If
    End Sub
    Public Shared Function getIndexToClasser(classer As String)
        Dim getIndexOfClasser = 0

        Dim counter = 0
        For Each i As String In classers.Items
            If i.Substring(0, i.IndexOf("µ")) = classer.ToLower Then
                getIndexOfClasser = counter
                Exit For
            End If
            counter += 1
        Next
        Return getIndexOfClasser
    End Function
    Public Shared Function VariableExists(variable As String, classer As String) As Boolean
        If ClasserExists(classer) Then
            Dim getindexofclasser = getIndexToClasser(classer.ToLower)
            For countVariables = 1 To classers.Items(getIndexOfClasser).ToString.Split("µ").Count - 1
                Dim getVar = classers.Items(getindexofclasser).ToString.Split("µ")(countVariables)
                'MsgBox("variable: " & variable & " / " & getVar.ToLower)
                If variable = getVar.ToLower Then
                    Return True
                    Exit Function
                End If
            Next
        Else
            'pup error cause the classer doesn't exists
        End If
        Return False
    End Function
    Public Shared Function GetVariable(name As String, classer As String) As String
        If VariableExists(name.ToLower, classer.ToLower) Then
            Dim getClasserBar = classers.Items(getIndexToClasser(classer.ToLower)).Substring(0, classers.Items(getIndexToClasser(classer.ToLower)).ToString.Length - 1)
            Dim getValuesBars = classersVariablesValues.Items(getIndexToClasser(classer.ToLower)).ToString
            Dim getValuesBar = ""
            If getValuesBars.StartsWith("µ") Then
                getValuesBar = getValuesBars.Substring(1)
            Else
                getValuesBar = getValuesBars
            End If
            '   MsgBox("getclasserbar: " & getClasserBar)
            '  MsgBox("getvaluesbar: " & getValuesBar)
            Dim getIndexFirst = 1
            For i = 1 To getClasserBar.ToString.Split("µ").Count
                Dim getVar = getClasserBar.ToString.Split("µ")(i)
                ' MsgBox("getvar: " & getVar & " / " & name.ToLower)
                If getVar = name.ToLower Then
                    getIndexFirst = i
                    Exit For
                End If
            Next
            Dim getValueOfIndex = getValuesBar.ToString.Split("µ")(getIndexFirst - 1)
            Return getValueOfIndex
        Else
            '   MsgBox("var doesn't exists")
            'pup error cause variable doesn't exists
        End If
    End Function
    Public Shared Sub CreateClasser(classer As String)
        If Not ClasserExists(classer.ToLower) Then
            classers.Items.Add(classer.ToLower & "µ")
            classersVariablesValues.Items.Add("")
        Else
            'pup error cause classer 'classer' is already existing
        End If
    End Sub
    Public Shared Sub EditVariable(variablename As String, newValue As String, classer As String)
        If ClasserExists(classer.ToLower) Then
            If VariableExists(variablename.ToLower, classer.ToLower) Then

                Dim getIndexOfClasser = getIndexToClasser(classer.ToLower)
                Dim getIndexOfNames = classers.Items(getIndexOfClasser)
                Dim getIndexOfValues = classersVariablesValues.Items(getIndexOfClasser)
                Dim getIndexMinusOne = 0
                For i = 1 To getIndexOfNames.ToString.Split("µ").Count - 1
                    Dim getVar = getIndexOfNames.ToString.Split("µ")(i)
                    If getVar.ToLower = variablename.ToLower Then
                        getIndexMinusOne = i
                    End If
                Next
                Dim finalBarToPut = ""
                For iss = 0 To getIndexOfValues.ToString.Split("µ").Count - 1
                    '  MsgBox("iss: " & iss & " / " & getIndexMinusOne - 1)
                    If iss = (getIndexMinusOne) Then
                        'MsgBox("newvalue: " & newValue)
                        finalBarToPut += "µ" & newValue
                    Else
                        finalBarToPut += "µ" & getIndexOfValues.ToString.Split("µ")(iss)
                    End If
                Next
                '  If finalBarToPut.EndsWith("µ") Then
                ' finalBarToPut = finalBarToPut.Substring(0, finalBarToPut.Length - 1)
                'End If
                If finalBarToPut.StartsWith("µ") Then
                    finalBarToPut = finalBarToPut.Substring(1)
                End If
                ' MsgBox("finalbar: " & finalBarToPut)
                '  MsgBox(classersVariablesValues.Items(getIndexOfClasser))
                '  MsgBox(classers.Items(getIndexOfClasser))

                Dim saveClasserFirst = classers.Items(getIndexOfClasser)
                classers.Items.RemoveAt(getIndexOfClasser)
                    classersVariablesValues.Items.RemoveAt(getIndexOfClasser)


                classers.Items.Add(saveClasserFirst)
                classersVariablesValues.Items.Add(finalBarToPut)
                End If
            Else
            'pup error cause classer 'classer' doesn't exists
        End If
    End Sub
    Public Shared Function ClasserExists(classer As String) As Boolean
        For Each i As String In classers.Items
            ' MsgBox("classeritem : " & i)
            If i.Substring(0, i.IndexOf("µ")) = classer Then
                Return True
                Exit Function
            End If
        Next
        Return False
    End Function
    Public Shared Sub start(line As String)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("define") Then
            Dim lineSubstring = FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(6)
            Dim classer = FormatConverters.ConvertToAbleToRead(lineSubstring.Split(" ")(0)).ToLower
            If Variables.ClasserExists(classer.ToLower) Then
                Dim varName = FormatConverters.getExpression(line.Split(" ")(1)).ToLower


                If line.Contains("=") Then
                    Dim varValue = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line.Split("=")(1))))

                    Variables.CreateVariable(varName, varValue, classer)

                Else
                    Variables.CreateVariable(varName, "", classer)
                End If
            Else
                'pup error cause classer isn't existing
            End If
        End If
    End Sub
    Public Shared Function GetClasserVariables(classer As String, delimiter As String) As String
        ' If ClasserExists(classer) Then
        ' Dim ClasserGoToIndex = classersValues.Items(classers.Items.IndexOf(classer))
        '  Dim FinalResult = ""
        '   For Each i As String In ClasserGoToIndex.split("[[lznDelimiterss3659]]")
        '    If i.Contains("[[LznDelimiterssVariable987]]") Then
        '     FinalResult += i.Split("[[LznDelimiterssVariable987]]")(0) & delimiter
        '      End If
        '      Next
        '     Return FinalResult
        '     Exit Function
        '     Else
        'pup error cause classer is not existing
        '    End If
        '     Return ""
    End Function
End Class