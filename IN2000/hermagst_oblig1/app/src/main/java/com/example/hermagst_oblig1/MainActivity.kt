package com.example.hermagst_oblig1

import PalindromeScreen
import QuizScreen
import UnitConverterScreen
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.material.MaterialTheme
import androidx.compose.material.Surface
import androidx.compose.material.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalFocusManager
import androidx.compose.ui.tooling.preview.Preview
import androidx.navigation.compose.NavHost
import androidx.navigation.compose.composable
import androidx.navigation.compose.rememberNavController
import com.example.hermagst_oblig1.ui.theme.Hermagst_oblig1Theme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            Hermagst_oblig1Theme {
                Surface(modifier = Modifier.fillMaxSize(), color = MaterialTheme.colors.background) {
                    val navController = rememberNavController()
                    NavHost(
                        navController = navController,
                        startDestination = Screens.PalindromeScreen.name
                    ) {
                        composable(Screens.PalindromeScreen.name) {
                            PalindromeScreen(navController = navController)
                        }
                        composable(Screens.UnitConverterScreen.name) {
                            UnitConverterScreen(navController = navController)
                        }
                        composable(Screens.QuizScreen.name) {
                            QuizScreen()
                        }
                    }
                }
            }
        }
    }
}

enum class Screens() {
    PalindromeScreen,
    UnitConverterScreen,
    QuizScreen
}