Public Class Returns
    Public Shared Sub start(line, linescounter, usedlinesforfunctions)
        If FormatConverters.removeSpacesAtBeginningAndEnd(line).ToLower.StartsWith("return") Then
            If usedlinesforfunctions.Items.Contains(linescounter + 1) Then
                Dim returnOfFunction = FormatConverters.getExpression(FormatConverters.ConvertToAbleToRead(FormatConverters.removeSpacesAtBeginningAndEnd(FormatConverters.removeSpacesAtBeginningAndEnd(line).Substring(6))))
                Dim FunctionName = Functions.getNameOfFunction(linescounter + 1).ToString.ToLower
                Dim indexOfFunctionName = Functions.listOfFunctionNamesForReturn.Items.IndexOf(FunctionName)
                Dim saveListOfReturns = Functions.listOfFunctionReturns.Items(indexOfFunctionName)
                Dim saveArguments = Functions.listOfFunctionsArguments.Items(indexOfFunctionName)
                Functions.listOfFunctionNamesForReturn.Items.RemoveAt(Functions.listOfFunctionNamesForReturn.Items.IndexOf(FunctionName))
                Functions.listOfFunctionReturns.Items.RemoveAt(indexOfFunctionName)
                Functions.listOfFunctionsArguments.Items.RemoveAt(indexOfFunctionName)
                Functions.listOfFunctionNamesForReturn.Items.Add(FunctionName)
                Functions.listOfFunctionReturns.Items.Add(saveListOfReturns & returnOfFunction & "µ")
                Functions.listOfFunctionsArguments.Items.Add(saveArguments)
                ' Functions.listOfFunctionReturns.Items.Add(returnOfFunction)
            End If
        End If
    End Sub
End Class