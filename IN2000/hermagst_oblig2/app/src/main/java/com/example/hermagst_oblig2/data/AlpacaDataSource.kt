package com.example.hermagst_oblig2.data

import com.example.hermagst_oblig2.model.AlpacaParty
import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.plugins.contentnegotiation.*
import io.ktor.client.request.*
import io.ktor.serialization.gson.*


class AlpacaDataSource(private val path: String) {
    private val client = HttpClient() {
        install(ContentNegotiation) {
            gson()
        }
    }

    suspend fun fetchParties(): List<AlpacaParty> {
        return client.get(path).body()
    }
}