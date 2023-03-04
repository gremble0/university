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
    private val alpacaDataSource = AlpacaDataSource(
        "https://in2000-proxy.ifi.uio.no/alpacaapi/alpacaparties"
    )
    private val votesDataSource = AlpacaDataSource(
        "https://in2000-proxy.ifi.uio.no/alpacaapi/district1"
    )
    var votes: Map<String, Int> = mutableMapOf()
    private var parties: List<AlpacaParty> = mutableListOf()
    private val _alpacaUiState = MutableStateFlow(AlpacaUiState(parties=listOf(), votes))
    val alpacaUiState: StateFlow<AlpacaUiState> = _alpacaUiState.asStateFlow()

    init {
        loadParties()
        loadVotes()
    }

    private fun loadParties() {
        viewModelScope.launch {
            parties = alpacaDataSource.fetchParties()
            _alpacaUiState.value = AlpacaUiState(parties=parties, votes)
        }
    }

    private fun loadVotes() {
        viewModelScope.launch {
            votes = votesDataSource.fetchJsonVotes()
            _alpacaUiState.value = AlpacaUiState(parties=parties, votes)
        }
    }
}