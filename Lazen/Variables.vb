Public Class Variables
    Public Shared classers As New ListBox
    Public Shared classersVariablesValues As New ListBox
    'on a classer value: classernameµvarµvarµvar
    'on a classer variablesvalues: varvalueµvarvalueµvarvalue
    Public Shared Sub CreateVariable(variable As String, variableValue As String, classer As String)

        If ClasserExists(classer.ToLower) Then

            If Not VariableExists(variable.ToLower, classer.ToLower) Then

                If CharacterVerification.Verify(variable) <> "ok" Then
                    'pup error cause invalid characters in voidname : CharacterVerification.Verify(variable).Split("-")(1)
                    Exit Sub
                End If


                Dim getindexofclasser As Long = getIndexToClasser(classer.ToLower)

                Dim SaveClasserContent As String = classers.Items(getindexofclasser)
                Dim SaveVariablesValuesContent As String = classersVariablesValues.Items(getindexofclasser)
                Dim getIndexToRemove As Long = getindexofclasser

                classers.Items.RemoveAt(getIndexToRemove)
                classersVariablesValues.Items.RemoveAt(getIndexToRemove)

                classers.Items.Add(SaveClasserContent & variable.ToLower & "µ")
                classersVariablesValues.Items.Add(SaveVariablesValuesContent & "µ" & FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(variableValue))))

            Else
                'pup error cause a variable called 'variable' already exists
            End If

        Else
            'pup error cause classer doesn't exists
        End If
    End Sub
    Public Shared Function getIndexToClasser(classer As String) As Long
        Dim getIndexOfClasser As Long = 0
        Dim counter As Long = 0

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

            For countVariables As Long = 1 To classers.Items(getindexofclasser).ToString.Split("µ").Count - 1

                Dim getVar As String = classers.Items(getindexofclasser).ToString.Split("µ")(countVariables)

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

            Dim getClasserBar As String = classers.Items(getIndexToClasser(classer.ToLower)).Substring(0, classers.Items(getIndexToClasser(classer.ToLower)).ToString.Length - 1)
            Dim getValuesBars As String = classersVariablesValues.Items(getIndexToClasser(classer.ToLower)).ToString
            Dim getValuesBar As String = ""
            Dim getIndexFirst As Long = 1

            If getValuesBars.StartsWith("µ") Then
                getValuesBar = getValuesBars.Substring(1)
            Else
                getValuesBar = getValuesBars
            End If

            For i As Long = 1 To getClasserBar.ToString.Split("µ").Count

                Dim getVar As String = getClasserBar.ToString.Split("µ")(i)

                If getVar = name.ToLower Then
                    getIndexFirst = i
                    Exit For
                End If

            Next

            Return getValuesBar.ToString.Split("µ")(getIndexFirst - 1)
        Else
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

                Dim getIndexOfClasser As Long = getIndexToClasser(classer.ToLower)
                Dim getIndexOfNames As String = classers.Items(getIndexOfClasser)
                Dim getIndexOfValues As String = classersVariablesValues.Items(getIndexOfClasser)
                Dim getIndexMinusOne As Long = 0

                For i As Long = 1 To getIndexOfNames.ToString.Split("µ").Count - 1

                    Dim getVar As String = getIndexOfNames.ToString.Split("µ")(i)

                    If getVar.ToLower = variablename.ToLower Then
                        getIndexMinusOne = i
                    End If

                Next
                Dim finalBarToPut As String = ""

                For iss As Long = 0 To getIndexOfValues.ToString.Split("µ").Count - 1

                    If iss = (getIndexMinusOne) Then
                        finalBarToPut += "µ" & newValue
                    Else
                        finalBarToPut += "µ" & getIndexOfValues.ToString.Split("µ")(iss)
                    End If

                Next

                If finalBarToPut.StartsWith("µ") Then
                    finalBarToPut = finalBarToPut.Substring(1)
                End If

                ''''''''''''''

                Dim saveClasserFirst As String = classers.Items(getIndexOfClasser)
                classers.Items.RemoveAt(getIndexOfClasser)
                classersVariablesValues.Items.RemoveAt(getIndexOfClasser)

                ''''''''''''''

                classers.Items.Add(saveClasserFirst)
                classersVariablesValues.Items.Add(finalBarToPut)

            End If
        Else
            'pup error cause classer 'classer' doesn't exists
        End If
    End Sub
    Public Shared Function ClasserExists(classer As String) As Boolean

        For Each i As String In classers.Items

            If i.Substring(0, i.IndexOf("µ")) = classer Then
                Return True
                Exit Function
            End If

        Next

        Return False
    End Function
    Public Shared Sub start(line As String)

        If FormatConverters.getBeforeParenthesis(FormatConverters.removeSpacesAtBeginningAndEnd(line)).ToLower = "define" Then

            Dim lineSubstring As String = FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(6)
            Dim classer As String = FormatConverters.ConvertToAbleToRead(lineSubstring.Split(" ")(0)).ToLower

            If Variables.ClasserExists(classer.ToLower) Then

                Dim varName As String = FormatConverters.getExpression(line.Split(" ")(1)).ToLower

                If VariableExists(varName, classer) Then
                    'pup error cause variable varName;;classer already exists
                    Exit Sub
                End If

                If line.Contains("=") Then
                    Dim varValue As String = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(line.Split("=")(1))))
                    Variables.CreateVariable(varName, varValue, classer)
                Else
                    Variables.CreateVariable(varName, "", classer)
                End If

            Else
                'pup error cause classer isn't existing
            End If

        End If

    End Sub
End Class