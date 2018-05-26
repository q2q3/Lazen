Public Class Form1
    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Interpret.entireCode = RichTextBox1.Text.Replace(vbTab, "")
        Interpret.Start(RichTextBox1.Text)
        Variables.classers.Items.Clear()
        '  Variables.classersVariablesNames.Items.Clear()
        Variables.classersVariablesValues.Items.Clear()
        Voids.ListOfCodeOfVoids.Items.Clear()
        Voids.ListOfVoidNames.Items.Clear()
        Voids.ListOfVoidVariables.Items.Clear()
        Interpret.UsedFunctionsListBox.Items.Clear()
        Interpret.UsedFunctionsInConditions.Items.Clear()
        Interpret.UsedLinesForFunctions.Items.Clear()
        Functions.listOfFunctionsNames.Items.Clear()
        Functions.listOfFunctionNamesForReturn.Items.Clear()
        Functions.listOfFunctionReturns.Items.Clear()
        Functions.listOfFunctionsArguments.Items.Clear()
        Functions.listOfFunctionsCodes.Items.Clear()
        Functions.listOfFunctionsNames.Items.Clear()
        Functions.listOfLines.Items.Clear()
        Functions.lineStartForCopy.Items.Clear()
    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
    End Sub
End Class