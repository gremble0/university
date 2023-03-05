package com.example.hermagst_oblig2.viewmodels

import androidx.lifecycle.ViewModel
import androidx.lifecycle.viewModelScope
import com.example.hermagst_oblig2.data.AlpacaDataSource
import com.example.hermagst_oblig2.data.AlpacaUiState
import com.example.hermagst_oblig2.model.AlpacaParty
import io.ktor.util.*
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.launch

class AlpacaViewModel: ViewModel() {
    private var parties: List<AlpacaParty> = mutableListOf()
    var votes: Map<String, Int> = mutableMapOf()
    private val baseUrl: String = "https://in2000-proxy.ifi.uio.no/alpacaapi/"

    private val _alpacaUiState = MutableStateFlow(AlpacaUiState(parties=listOf(), votes, "1"))
    val alpacaUiState: StateFlow<AlpacaUiState> = _alpacaUiState.asStateFlow()
    var district: String = alpacaUiState.value.district

    init {
        loadParties()
        loadVotes()
    }

    private fun loadParties() {
        viewModelScope.launch {
            val alpacaDataSource = AlpacaDataSource("${baseUrl}alpacaparties")
            parties = alpacaDataSource.fetchParties()
            _alpacaUiState.value = AlpacaUiState(parties, votes, district)
        }
    }

    fun loadVotes() {
        viewModelScope.launch {
            val votesDataSource = AlpacaDataSource("${baseUrl}district${district}")
            votes = if (district == "3") {
                votesDataSource.fetchXmlVotes()
            } else {
                votesDataSource.fetchJsonVotes()
            }
            _alpacaUiState.value = AlpacaUiState(parties, votes, district)
        }
    }
}