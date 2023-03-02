package com.example.hermagst_oblig2.viewmodels

import androidx.lifecycle.ViewModel
import androidx.lifecycle.viewModelScope
import com.example.hermagst_oblig2.data.AlpacaDataSource
import com.example.hermagst_oblig2.data.AlpacaUiState
import kotlinx.coroutines.flow.MutableStateFlow
import kotlinx.coroutines.flow.StateFlow
import kotlinx.coroutines.flow.asStateFlow
import kotlinx.coroutines.launch

class AlpacaViewModel: ViewModel() {
    private val alpacaDataSource = AlpacaDataSource(": https://in2000-proxy.ifi.uio.no/alpacaapi/alpacaparties.")
    private val _alpacaUiState = MutableStateFlow(AlpacaUiState(parties=listOf()))
    val alpacaUiState: StateFlow<AlpacaUiState> = _alpacaUiState.asStateFlow()

    private fun loadParties() {
        viewModelScope.launch {
            val parties = alpacaDataSource.fetchParties()
            _alpacaUiState.value = AlpacaUiState(parties=parties)
        }
    }
}