package com.example.hermagst_oblig2.screens

import androidx.compose.foundation.background
import androidx.compose.foundation.layout.*
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.itemsIndexed
import androidx.compose.foundation.shape.CircleShape
import androidx.compose.material.*
import androidx.compose.material3.ElevatedCard
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.runtime.*
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.draw.clip
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.layout.ContentScale
import androidx.compose.ui.unit.dp
import androidx.lifecycle.viewModelScope
import androidx.lifecycle.viewmodel.compose.viewModel
import coil.compose.AsyncImage
import com.example.hermagst_oblig2.model.AlpacaParty
import com.example.hermagst_oblig2.viewmodels.AlpacaViewModel
import kotlinx.coroutines.launch
import kotlin.math.roundToInt

@OptIn(ExperimentalMaterialApi::class)
@Composable
fun AlpacaScreen(alpacaViewModel: AlpacaViewModel = viewModel()) {
    val alpacaUiState by alpacaViewModel.alpacaUiState.collectAsState()
    val districts = listOf("1", "2", "3")

    var expanded by remember { mutableStateOf(false) }

    Column(
        modifier = Modifier.fillMaxWidth(),
        verticalArrangement = Arrangement.Center,
        horizontalAlignment = Alignment.CenterHorizontally
    ) {
        ExposedDropdownMenuBox(
            expanded = expanded,
            onExpandedChange = { expanded = !expanded }
        ) {
            TextField(
                readOnly = true,
                value = alpacaUiState.district,
                onValueChange = {},
                label = { Text("Velg valgdistrikt") },
                trailingIcon = { ExposedDropdownMenuDefaults.TrailingIcon(expanded = expanded) },
            )
            ExposedDropdownMenu(
                expanded = expanded,
                onDismissRequest = { expanded = false }
            ) {
                districts.forEach { selectionOption ->
                    DropdownMenuItem(
                        onClick = {
                            // Laster data fra api hver gang knapp trykkes. Kunne gjort det
                            // på en annen måte men dette var mer lik slik oppgaveteksen ba om
                            alpacaViewModel.district = selectionOption
                            alpacaViewModel.loadVotes()
                            expanded = false
                        }
                    ) {
                        Text(text = selectionOption)
                    }
                }
            }
        }

        LazyColumn {
            var votesTotal = 0.0
            alpacaUiState.votes.forEach { votesTotal += it.value }
            itemsIndexed(alpacaUiState.parties) { index, partyData ->
                val doublePercent: Double? = (alpacaUiState.votes[(index+1).toString()]?.toDouble()?.div(votesTotal))
                AlpacaCard(
                    alpacaParty = partyData,
                            "${alpacaUiState.votes[(index+1).toString()]} - ${
                            doublePercent
                            ?.times(100.0)
                            ?.roundToInt()
                            ?.div(1.0)
                            ?.toInt()
                            .toString()
                        }%"
                )
            }
        }
    }
}


@OptIn(ExperimentalMaterial3Api::class)
@Composable
fun AlpacaCard(alpacaParty: AlpacaParty, votes: String) {
    ElevatedCard(
        modifier = Modifier
            .fillMaxWidth()
            .padding(15.dp)
    ) {
        Column(
            verticalArrangement = Arrangement.Center,
            horizontalAlignment = Alignment.CenterHorizontally
        ) {
            Spacer(
                modifier = Modifier
                    .height(20.dp)
                    .fillMaxWidth()
                    .background(color = Color(android.graphics.Color.parseColor(alpacaParty.color)))
            )

            Text(text = alpacaParty.name)

            AsyncImage(
                model = alpacaParty.img,
                contentDescription = "${alpacaParty.name} leader ${alpacaParty.leader}",
                contentScale = ContentScale.Crop,
                modifier = Modifier
                    .size(200.dp)
                    .clip(CircleShape)
            )

            Text(text = "Leader: ${alpacaParty.leader}")

            Text(text = "Votes: $votes")
        }
    }
}