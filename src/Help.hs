module Help (detailedHelp)
where

import qualified Options.Applicative.Help.Pretty as P

detailedHelp :: Bool -> P.Doc
detailedHelp redhat =
  P.vcat
  [ P.text "Tool for generating bugzilla queries"
  , P.empty
  , P.text "FIELDS = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map P.text (if redhat then rhFields else generalFields))) <> P.rbrace)
  ]

-- FIXME: filter by @redhat.com
-- bugzilla query --json -b 1865911
rhFields :: [String]
rhFields =
--      "actual_time":
  "alias":
  "assigned_to":
  "blocks":
  "cc":
  "cf_approved_release":
  "cf_build_id":
  "cf_conditional_nak":
  "cf_cust_facing":
--  "cf_deadline":
  "cf_deadline_type":
  "cf_devdoctest":
  "cf_doc_type":
  "cf_environment":
  "cf_epm_pri":
--  "cf_final_deadline":
  "cf_internal_target_milestone":
  "cf_internal_target_release":
  "cf_partner":
  "cf_pgm_internal":
  "cf_pm_score":
  "cf_qe_conditional_nak":
  "cf_release_notes":
  "cf_target_upstream_version":
  "cf_type":
  "cf_verified":
  "classification":
  "component":
--  "creation_time":
  "creator":
  "depends_on":
  "devel_whiteboard":
  "docs_contact":
--  "estimated_time":
  "groups":
  "id":
  "internal_whiteboard":
  "keywords":
--  "last_change_time":
  "op_sys":
  "platform":
  "priority":
  "product":
  "qa_contact":
  "qa_whiteboard":
  "remaining_time":
  "resolution":
  "see_also":
  "severity":
  "status":
  "sub_component":
  "sub_components":
  "summary":
  "tags":
  "target_milestone":
  "target_release":
  "url":
  "version":
  []

generalFields :: [String]
generalFields =
  "alias" :
  "assigned_to" :
  "blocks" :
  "cc" :
  "cf_clone_of" :
  "cf_doc_type" :
  "cf_environment" :
  "cf_fixed_in" :
  "cf_last_closed" :
  "cf_release_notes" :
  "cf_type" :
  "classification" :
  "component" :
  "creation_time" :
  "creator" :
  "deadline" :
  "depends_on" :
  "docs_contact" :
  "dupe_of" :
  "groups" :
  "id" :
  "keywords" :
  "last_change_time" :
  "op_sys" :
  "platform" :
  "priority" :
  "product" :
  "qa_contact" :
  "resolution" :
  "see_also" :
  "severity" :
  "status" :
  "summary" :
  "target_milestone" :
  "target_release" :
  "url" :
  "version" :
  "whiteboard" :
  []
