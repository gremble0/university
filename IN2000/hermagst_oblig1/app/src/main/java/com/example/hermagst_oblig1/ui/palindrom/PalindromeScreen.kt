import androidx.compose.foundation.interaction.MutableInteractionSource
import androidx.compose.foundation.interaction.collectIsPressedAsState
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material.Button
import androidx.compose.material.Text
import androidx.compose.material.TextField
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.text.input.TextFieldValue
import androidx.navigation.NavController
import com.example.hermagst_oblig1.Screens

@Composable
fun PalindromeScreen(navController: NavController) {
    val focusManager = LocalFocusManager.current
    var inputText by remember { mutableStateOf("") }
    var outputText by remember { mutableStateOf("Skriv et ord") }
    Column(
            modifier = Modifier.fillMaxSize(),
            horizontalAlignment = Alignment.CenterHorizontally,
            verticalArrangement = Arrangement.SpaceBetween
    ) {
        TextField(
            label = { Text(text = "Skriv et palindrom") },
            singleLine = true,
            value = inputText,
            onValueChange = { newText ->
                inputText = newText
            }
        )

        Button(onClick = {
            focusManager.clearFocus(true)
            val isPalindrome = checkPalindrome(inputText)
            outputText = when (isPalindrome) {
                1 -> "$inputText er et palindrom"
                0 -> outputText
                else -> { "$inputText er ikke et palindrom" }
            }
            inputText = ""
        }) {
            Text(text = "Sjekk palindrom")
        }

        Text(text = outputText)

        Button(modifier = Modifier.fillMaxWidth(), onClick = {
            navController.navigate(Screens.UnitConverterScreen.name)
        }) {
            Text(text = "Gå til enhetskonverterer")
        }
    }
}


fun checkPalindrome(string: String): Int {
    if (string.isEmpty()) return 0
    val lower = string.lowercase()
    var reversed = ""
    for (i in lower.length - 1 downTo 0) {
        reversed += lower[i]
    }
    return if (reversed == lower) 1 else -1
}