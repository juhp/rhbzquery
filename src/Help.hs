{-# LANGUAGE CPP #-}

module Help (detailedHelp)
where

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid ((<>))
#endif
import qualified Options.Applicative.Help.Pretty as P

import ParseArg (statusList)

detailedHelp :: P.Doc
detailedHelp =
  P.vcat
  [ P.text "Tool for generating bugzilla queries"
  , P.empty
  , P.text "FIELDS = " <> P.lbrace <> P.align (P.hsep (P.punctuate P.comma (map P.text allBzFields)) <> P.rbrace)
  , P.empty
  , P.text "STATUS = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map P.text (statusList ++ ["ALL"]))) <> P.rbrace)
  , P.empty
  , P.text "PRODUCTVERSION = " <> P.lbrace <> P.align (P.fillCat (P.punctuate P.comma (map P.text ["rawhide", "fedora", "fXY", "epel", "epelX", "rhel8", "rhel7", "rhelX.Z"])) <> P.rbrace)
  , P.empty
  , P.text "See https://github.com/juhp/rhbzquery#readme for examples"
  ]

-- FIXME: filter by @redhat.com
-- bugzilla query --json -b 1865911
allBzFields :: [String]
allBzFields =
-- from https://bugzilla.redhat.com/rest/field/bug
  ["agile_pool.name",
   "agile_team.name",
   "alias",
   "assigned_to",
   "assigned_to_realname",
   "attach_data.thedata",
   "attachments.description",
   "attachments.filename",
   "attachments.isobsolete",
   "attachments.ispatch",
   "attachments.isprivate",
   "attachments.mimetype",
   "attachments.submitter",
   "blocked",
   "bug_agile_pool.pool_id",
   "bug_agile_pool.pool_order",
   "bug_file_loc",
   "bug_group",
   "bug_id",
   "bug_severity",
   "bug_status",
   "cc",
   "cclist_accessible",
   "cf_approved_release",
   "cf_atomic",
   "cf_build_id",
   "cf_business_market_problem",
   "cf_category",
   "cf_clone_of",
   "cf_cloudforms_team",
   "cf_compliance_control_group",
   "cf_compliance_level",
   "cf_conditional_nak",
   "cf_crm",
   "cf_cust_facing",
   "cf_deadline",
   "cf_deadline_type",
   "cf_devdoctest",
   "cf_devel_whiteboard",
   "cf_doc_type",
   "cf_docs_score",
   "cf_documentation_action",
   "cf_environment",
   "cf_epm_cdp",
   "cf_epm_phd",
   "cf_epm_prf_state",
   "cf_epm_pri",
   "cf_epm_ptl",
   "cf_epm_put",
   "cf_final_deadline",
   "cf_fixed_in",
   "cf_internal_target_milestone",
   "cf_internal_target_release",
   "cf_internal_whiteboard",
   "cf_last_closed",
   "cf_mount_type",
   "cf_ovirt_team",
   "cf_pgm_internal",
   "cf_pm_score",
   "cf_qa_whiteboard",
   "cf_qe_conditional_nak",
   "cf_regression_status",
   "cf_release_notes",
   "cf_srtnotes",
   "cf_story_points",
   "cf_target_upstream_version",
   "cf_type",
   "cf_verified_branch",
   "cf_zstream_target_release",
   "classification",
   "comment_tag",
   "commenter",
   "component",
   "content",
   "creation_ts",
   "days_elapsed",
   "deadline",
   "delta_ts",
   "dependent_products",
   "dependson",
   "docs_contact",
   "docs_contact_realname",
   "estimated_time",
   "everconfirmed",
   "ext_bz_bug_map.ext_bz_bug_id",
   "ext_bz_bug_map.ext_status",
   "external_bugzilla.description",
   "external_bugzilla.url",
   "extra_components",
   "extra_versions",
   "flagtypes.name",
   "keywords",
   "last_visit_ts",
   "longdesc",
   "longdescs.count",
   "longdescs.isprivate",
   "op_sys",
   "owner_idle_time",
   "percentage_complete",
   "priority",
   "product",
   "qa_contact",
   "qa_contact_realname",
   "remaining_time",
   "rep_platform",
   "reporter",
   "reporter_accessible",
   "reporter_realname",
   "requestees.login_name",
   "resolution",
   "rh_rule",
   "rh_sub_components",
   "see_also",
   "setters.login_name",
   "short_desc",
   "status_whiteboard",
   "tag",
   "target_milestone",
   "target_release",
   "version",
   "view",
   "votes",
   "work_time"]
