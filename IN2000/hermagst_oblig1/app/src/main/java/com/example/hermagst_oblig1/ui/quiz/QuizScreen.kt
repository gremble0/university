import android.graphics.Color
import android.graphics.Paint.Align
import androidx.compose.foundation.layout.*
import androidx.compose.material.Button
import androidx.compose.material.ButtonDefaults
import androidx.compose.material.Text
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.unit.dp
import androidx.navigation.NavController
import com.example.hermagst_oblig1.Screens

@Composable
fun QuizScreen() {
    val quizState = QuizUiState(0)
    var currentQuestion by remember { mutableStateOf(quizState.questions[0]) }
    var score by remember { mutableStateOf(0) }
    var scoreText by remember { mutableStateOf("Du har $score poeng") }
    var counter by remember { mutableStateOf(0) }

    val buttonColorTrue =   if (counter < quizState.questions.size)
                                ButtonDefaults.buttonColors(backgroundColor = androidx.compose.ui.graphics.Color.Green)
                            else
                                ButtonDefaults.buttonColors(backgroundColor = androidx.compose.ui.graphics.Color.Transparent)

    val buttonColorFalse = if (counter < quizState.questions.size)
                                ButtonDefaults.buttonColors(backgroundColor = androidx.compose.ui.graphics.Color.Red)
                            else
                                ButtonDefaults.buttonColors(backgroundColor = androidx.compose.ui.graphics.Color.Transparent)

    Column(
        modifier = Modifier.fillMaxSize(),
        horizontalAlignment = Alignment.CenterHorizontally,
        verticalArrangement = Arrangement.SpaceBetween
    ) {
        Text(text = currentQuestion.question)

        Row(
            horizontalArrangement = Arrangement.spacedBy(50.dp)
        ) {
            Button(
                onClick = {
                    try {
                        if(quizState.questions[counter].isTrue) {
                            score++
                            scoreText = "Du har $score poeng"
                        }
                        counter++
                        currentQuestion = quizState.questions[counter]
                    } catch (e: IndexOutOfBoundsException) {
                        scoreText = "Poeng: $score / ${quizState.questions.size}"
                    }
                },
                colors = buttonColorTrue
            ) {
                Text(text = "Fakta")
            }
            Button(
                onClick = {
                    try {
                        if(!quizState.questions[counter].isTrue) {
                            score++
                            scoreText = "Du har $score poeng"
                        }
                        counter++
                        currentQuestion = quizState.questions[counter]
                    } catch (e: IndexOutOfBoundsException) {
                        scoreText = "Poeng: $score / ${quizState.questions.size}"
                    }
                },
                colors = buttonColorFalse
            ) {
                Text(text = "Fleip")
            }
        }

        Text(text = scoreText)

        Button(
            modifier = Modifier.fillMaxWidth(),
            onClick = {
                score = 0
                counter = 0
                scoreText = "Du har 0 poeng"
                currentQuestion = quizState.questions[0]
            }
        ) {
            Text(text = "Restart quiz")
        }
    }
}

data class QuizUiState(var currentQuestion: Int) {
    val questions: List<Question> = listOf(
        Question("Jorden er flat", false),
        Question("Sveriges høyeste fjell er høyere enn Norges", false),
        Question("Kong Harald V ble konge i 1991", true),
        Question("Danmark er større enn Norge", true)
    )

    fun getNextQuestion(): Question {
        // Virker ikke. currentQuestion telleren i QuizUiState er alltid 1 eller 2, funksjonen er ikke i bruk
        // Endte opp med å gjøre de fleste operasjonene i hovedfunksjonen så denne dataklassen er egentlig ganske ubrukelig
        currentQuestion += 1
        return questions[currentQuestion % questions.size]
    }
}

data class Question(val question: String, val isTrue: Boolean)