package com.example.hermagst_oblig2.data

import com.example.hermagst_oblig2.model.AlpacaParty

data class AlpacaUiState(
    val parties: List<AlpacaParty>,
    val votes: Map<String, Int>,
    var district: String
)