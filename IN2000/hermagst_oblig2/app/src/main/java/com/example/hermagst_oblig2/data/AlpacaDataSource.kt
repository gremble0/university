package com.example.hermagst_oblig2.data

import com.example.hermagst_oblig2.model.AlpacaParty
import com.example.hermagst_oblig2.model.AlpacaPartyList
import io.ktor.client.*
import io.ktor.client.call.*
import io.ktor.client.plugins.contentnegotiation.*
import io.ktor.client.request.*
import io.ktor.serialization.gson.*
import org.json.JSONArray
import org.json.JSONObject

class AlpacaDataSource(private val path: String) {

    private val client = HttpClient {
        install(ContentNegotiation) {
            gson()
        }
    }

    suspend fun fetchParties(): List<AlpacaParty> {
        val body: AlpacaPartyList = client.get(path).body()
        return body.parties

//        Fikk ingen av de innebygde ktor/gson deserialiseringsmetodene til å virke
//        med strukturen til apiet. Ga opp og deserialiserte det selv
//
//        val parties = mutableListOf<AlpacaParty>()
//        val stringBody: String = client.get(path).body()
//        val obj = JSONObject(stringBody)
//        val partiesArr = obj.getJSONArray("parties")
//
//        for (i in 0 until partiesArr.length()) {
//            val party = partiesArr.getJSONObject(i)
//            parties.add(
//                AlpacaParty(
//                    party["id"] as String,
//                    party["name"] as String,
//                    party["leader"] as String,
//                    party["img"] as String,
//                    party["color"] as String
//                )
//            )
//        }
//
//        return parties
    }

    suspend fun fetchJsonVotes(): Map<String, Int> {
        val res = mutableMapOf(
            "1" to 0,
            "2" to 0,
            "3" to 0,
            "4" to 0
        )

        val responseString: String = client.get(path).body()
        val responseArr = JSONArray(responseString)

        for (i in 0 until responseArr.length()) {
            val vote = responseArr.getJSONObject(i)
            res[vote["id"] as String] = res[vote["id"] as String]!! + 1 // += 1
        }
        return res
    }
}