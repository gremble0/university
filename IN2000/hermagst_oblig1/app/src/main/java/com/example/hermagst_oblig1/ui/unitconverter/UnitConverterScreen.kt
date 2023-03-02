import androidx.compose.foundation.layout.*
import androidx.compose.foundation.text.KeyboardOptions
import androidx.compose.material.*
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.text.input.KeyboardType
import androidx.navigation.NavController
import com.example.hermagst_oblig1.Screens
import kotlinx.coroutines.launch
import kotlin.math.roundToInt

@OptIn(ExperimentalMaterialApi::class)
@Composable
fun UnitConverterScreen(navController: NavController) {
    val imperialUnits = listOf("Fluid ounces", "Cups", "Gallons", "Hogsheads")
    val focusManager = LocalFocusManager.current
    val snackbarHostState = remember { SnackbarHostState() }
    val coScope = rememberCoroutineScope()

    var inputText by remember { mutableStateOf("") }
    var outputText by remember { mutableStateOf("") }
    var expanded by remember { mutableStateOf(false) }
    var selectedUnit by remember { mutableStateOf(imperialUnits[0]) }
    Scaffold(
        snackbarHost = { SnackbarHost(hostState = snackbarHostState) },
        content = { innerPadding ->
            Column(
                modifier = Modifier
                    .padding(innerPadding)
                    .fillMaxSize(),
                horizontalAlignment = Alignment.CenterHorizontally,
                verticalArrangement = Arrangement.SpaceBetween
            ) {
                TextField(
                    keyboardOptions = KeyboardOptions(keyboardType = KeyboardType.Number),
                    label = { Text(text = "Skriv antall imperiske enheter å oversette") },
                    singleLine = true,
                    value = inputText,
                    onValueChange = { newText ->
                        inputText = newText
                    }
                )

                ExposedDropdownMenuBox(
                    expanded = expanded,
                    onExpandedChange = { expanded = !expanded }
                ) {
                    TextField(
                        readOnly = true,
                        value = selectedUnit,
                        onValueChange = {},
                        label = { Text("Label") },
                        trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded) },
                    )
                    ExposedDropdownMenu(
                        expanded = expanded,
                        onDismissRequest = { expanded = false }
                    ) {
                        imperialUnits.forEach { selectionOption ->
                            DropdownMenuItem(
                                onClick = {
                                    selectedUnit = selectionOption
                                    expanded = false
                                }
                                // contentPadding = ExposedDropdownMenuDefaults.ItemContentPadding
                            ) {
                                Text(text = selectionOption)
                            }
                        }
                    }
                }

                Button(
                    onClick = {
                        try {
                            outputText = when (selectedUnit) {
                                // litt stygt
                                "Fluid ounces" -> "$inputText $selectedUnit er ${(inputText.toDouble() * 0.02957 * 100.0).roundToInt() / 100.0} liter."
                                "Cups" -> "$inputText $selectedUnit er ${(inputText.toDouble() * 0.23659 * 100.0).roundToInt() / 100.0} liter."
                                "Gallons" -> "$inputText $selectedUnit er ${(inputText.toDouble() * 3.78541 * 100.0).roundToInt() / 100.0} liter."
                                else -> {
                                    "$inputText $selectedUnit er ${(inputText.toDouble() * 238.481 * 100.0).roundToInt() / 100.0} liter."
                                }
                            }
                            inputText = ""
                            focusManager.clearFocus(true)
                        } catch(e: java.lang.NumberFormatException) {
                            coScope.launch {
                                snackbarHostState.showSnackbar(
                                    message = "Ugyldig input",
                                    duration = SnackbarDuration.Short
                                )
                            }
                        }
                    }
                ) {
                    Text(text = "Konverter")
                }

                Text(text = outputText)

                Button(modifier = Modifier.fillMaxWidth(), onClick = {
                    navController.navigate(Screens.QuizScreen.name)
                }) {
                    Text(text = "Gå til Quiz")
                }
            }
        }
    )
}