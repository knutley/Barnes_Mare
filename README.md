This is the Git for Nicholas Barnes's Resident Denunciation Project. I have included the visual renderings I did for him (the annotations for which are yet to be cleaned up) and the code for the structural break test. We will include replication data in time. That the data has not yet been included is due to its sensitive nature and our ongoing discussions about best anonymisation practices (i.e. those practices which offer insight to other researchers, while maintaining confidentiality).  


Codebook 
- Codigo.Unico: the unique code for each observation (or recorded denunciation)
- den_cd: denunciation code
- den_numero: denunciation number
- den_dt_rec: time and date of denunciation call
- den_dt_alt:  if there’s a follow-up denunciation, this is the date and time
- Comunidades..Guia.de.Ruas.: we created this variable as a first attempt to geolocate them
- den_op_rec: operator number
- den_logr_manual: {not sure about this one—I think it refers to whether the location was manually entered or not}
- den_logr_tp: type of location (street, favela, alley, etc.)
- den_logr_ds: name of the location (street, alley name etc.)
- den_logr_num: number
- den_comunidade: (this is a variable we created by going line by line and attributing each denunciation to a favela neighborhood within Maré)
- den_logr_cmpl: complement to the location (apartment or house number)
- den_logr_bairro: neighborhood
- den_logr_subbairro: sub-neighborhood
- den_logr_mun: municipality
- den_logr_uf: federal state
- den_logr_cep: zip code (CEP)
- den_loc_ref: local reference
- den_xpto: this is a special category that DD uses for internal purposes
- den_class: this is a classification, not sure what it means
- den_texto: small blurb about the call on the denunciation; sometimes includes actors, location, previous crime, etc.
- den_versao: denunciation version—doesn’t seem to correspond to anything
- den_corr_cd: denunciation correction
- den_imediata: does the denunciation require immediate action
- den_redifundir: redistributed? I think this has to do with whether or not the denunciation was sent to others
- env_cd: suspect number
- env_den_cd: denunciation code
- env_usu_cd: user code
- env_nome: suspect name
- env_vulgo: the nicknames of the people that were denounced
- env_end_tp: suspect address type
- env_logr_tp: suspect location type
- env_logr_ds: suspect address name
- env_logr_num: suspect address number
- env_logr_cmpl: suspect address complement
- env_logr_bairro: suspect address neighborhood
- env_logr_subbairo: suspect address sub-neighborhood
- env_logr_mun: suspect address municipality
- env_logr_uf: suspect address federal state
- env_loc_ref: suspect address reference
- env_sexo: suspect sex
- env_idade: suspect age
- env_pele: suspect race
- env_estatura: suspect height
- env_olhos: suspect eye color
- env_cabelo: suspect hair color
- env_porte: suspect weight
- env_caract: suspect characteristics
- classificacao: classification (insufficient data, important, normal
- assunto_classe: broad class of crime
- assunto_tipo: narrow class of crime
- xpt_ds: special category (illicit balloons, drug fair, special suspects)
- red_cd: police contact number
- red_den_cd: police contact code
- red_rtp_cd: {not sure about this one}
- red_resp_data: police responded date
- red_oper_data: operator contacted date
- red_cad_data: registered date
- red_usu_cd: user code
- red_relato: report of what happened when police contacted
- red_ext_cd: extension code
- red_oet_cd: {not sure about this one}
- den_operacao: police operation (binary)
- den_boca: was a boca de fumo mentioned (binary)
- den_contra: against an OCG
- den_sassa: against Sassa (binary)
- den_linho: against Linho (binary)
- den_facao: against Facão (binary)
- den_menorp: against Menor P (binary)
- den_MB: against MB (binary)
- den_kito: against Kito (binary)
- den_motoboy: against Moto Boy (binary)
- den_LC: against LC (binary)
- den_gordo: against Gordo (binary)
- den_alvarenga: against Alvarenga (binary)
- den_zangado: against Zangado (binary)
- den_milicia: against Milícia (binary)
- date: cleaned date column 
- Ano.de.Den.Dt.Rec: (unclear)
- Mes.de.Den.Dt.Rec: (unclear)
- X: (unclear) 
- crime_type:this is a created variable with gang-related crimes (armas de fogo e artefatos explosivos, crimes contra a pessoa, crimes contra o patrimonio, and substancias entorpecentes) vs. broadly defined "other" crimes  
